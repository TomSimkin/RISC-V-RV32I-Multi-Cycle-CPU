library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cpu_top is
    generic (
        DATA_WIDTH : positive := 32;
        ADDR_WIDTH : positive := 8;
		INIT_FILE  : string   := "RAM.mem" -- Placeholder replaced in testbench
    );
    port (
        clk     : in  std_logic;
        rst_n   : in  std_logic;
        -- Debug outputs for write-back observation
        wb_en_dbg        : out std_logic;
        reg_write_en_dbg : out std_logic;
        rd_idx_dbg       : out std_logic_vector(4 downto 0);
        wb_data_dbg      : out std_logic_vector(DATA_WIDTH-1 downto 0);
        -- Trap (EBREAK) export for TB stop
        trap_dbg         : out std_logic
    );
end entity;

architecture rtl of cpu_top is
	-- Component decleration
	
	-- PC_UNIT
	component pc_unit
        generic (DATA_WIDTH : positive);
        port (
            clk, rst_n         						: in  std_logic;
            pc_sel             						: in  std_logic_vector(1 downto 0);
            pc_update_en                            : in  std_logic;
            branch_off, jal_off, jalr_rs1, jalr_off	: in  std_logic_vector(DATA_WIDTH-1 downto 0);
            pc_curr            						: out std_logic_vector(DATA_WIDTH-1 downto 0)
        );

    end component;
	
	-- REGISTER_FILE
	component register_file
        generic (DATA_WIDTH : positive; REG_COUNT : positive);
        port (
            clk              : in  std_logic;
            rs1_idx, rs2_idx : in  std_logic_vector(4 downto 0);
            rA_data, rB_data : out std_logic_vector(DATA_WIDTH-1 downto 0);
            rd_idx           : in  std_logic_vector(4 downto 0);
            rD_data          : in  std_logic_vector(DATA_WIDTH-1 downto 0);
            reg_write     	 : in  std_logic
        );
    end component;
	
	-- DECODER
	component decoder
		generic (DATA_WIDTH : positive);
		port (
			clk, enable           	: in  std_logic;
			instr         			: in  std_logic_vector(31 downto 0);
			rs1_idx       			: out std_logic_vector(4 downto 0);
			rs2_idx       			: out std_logic_vector(4 downto 0);
			rd_idx        			: out std_logic_vector(4 downto 0);
			imm_i         			: out std_logic_vector(DATA_WIDTH-1 downto 0);
			imm_s         			: out std_logic_vector(DATA_WIDTH-1 downto 0);
			imm_b         			: out std_logic_vector(DATA_WIDTH-1 downto 0);
			imm_u         			: out std_logic_vector(DATA_WIDTH-1 downto 0);
			imm_j         			: out std_logic_vector(DATA_WIDTH-1 downto 0);
			funct3        			: out std_logic_vector(2 downto 0);
			funct7_5      			: out std_logic;
			opcode        			: out std_logic_vector(6 downto 0);
			reg_write_en  			: out std_logic                    
		);
	end component;
	
	-- ALU
	component alu
        generic (DATA_WIDTH : positive);
        port (
            op_a, op_b : in  std_logic_vector(DATA_WIDTH-1 downto 0);
            funct3     : in  std_logic_vector(2 downto 0);
            funct7_5   : in  std_logic;
            result     : out std_logic_vector(DATA_WIDTH-1 downto 0)
        );
    end component;
	
	-- BRANCH COMPARE
	component branch_cmp
        generic (DATA_WIDTH : positive);
        port (
            rs1_val, rs2_val : in  std_logic_vector(DATA_WIDTH-1 downto 0);
            funct3           : in  std_logic_vector(2 downto 0);
            branch_taken     : out std_logic
        );
    end component;
	
	-- RAM
	component ram
        generic (
            ADDR_WIDTH : positive;
            DATA_WIDTH : positive;
			INIT_FILE  : string := "RAM.mem"
        );
        port (
            clk         : in  std_logic;
            addr        : in  std_logic_vector(ADDR_WIDTH-1 downto 0);
            wr_data     : in  std_logic_vector(DATA_WIDTH-1 downto 0);
            byte_enable : in  std_logic_vector(3 downto 0);
            mem_read    : in  std_logic;
            mem_write   : in  std_logic;
            rd          : out std_logic_vector(DATA_WIDTH-1 downto 0)
        );
    end component;
	
	-- CONTROL UNIT
	component control_unit
        port (
            clk, rst_n          											: in  std_logic;
            opcode              											: in  std_logic_vector(6 downto 0);
            funct3             												: in  std_logic_vector(2 downto 0);
            branch_taken       												: in  std_logic;
            
            -- Debug inputs
            pc_curr															: in std_logic_vector(31 downto 0);
            rs1_val															: in std_logic_vector(31 downto 0);
            rs2_val															: in std_logic_vector(31 downto 0);
            alu_result														: in std_logic_vector(31 downto 0);
            rd_idx															: in std_logic_vector(4 downto 0);

            fetch1_en, fetch2_en, decode_en, reg_en, exec_en, mem_en, wb_en : out std_logic;
            pc_sel             												: out std_logic_vector(1 downto 0);
			pc_update_en													: out std_logic;
			mem_read, mem_write    											: out std_logic;
			pipe_flush														: out std_logic;  -- Pipeline flush signal for branches
			trap															: out std_logic
        );
    end component;
	
	-- Internal signals
	
	-- PC UNIT
	signal pc_curr  	: std_logic_vector(DATA_WIDTH-1 downto 0);
	signal pc_sel   	: std_logic_vector(1 downto 0);
	signal pc_update_en : std_logic;
	
	-- REGISTER FILE
	signal rs1_val, rs2_val : std_logic_vector(DATA_WIDTH-1 downto 0);
	signal reg_write_en     : std_logic;  -- decoder output (DECODE stage)
	signal ram_data 		: std_logic_vector(DATA_WIDTH-1 downto 0); -- Raw data from RAM

	-- Load/Store effective address
	signal addr_ex_comb : std_logic_vector(DATA_WIDTH-1 downto 0);
	signal addr_mem     : std_logic_vector(DATA_WIDTH-1 downto 0);
	-- Unified RAM address (word index) used for both IF and MEM
	signal ram_addr     : std_logic_vector(ADDR_WIDTH-1 downto 0);
	-- Store byte enables and load data formatting
	signal be_mem       : std_logic_vector(3 downto 0);
	signal load_data_ext: std_logic_vector(DATA_WIDTH-1 downto 0);
	signal writeback_data: std_logic_vector(DATA_WIDTH-1 downto 0);
	-- Store data aligned to byte/halfword lane per addr_mem
	signal wr_data_mem  : std_logic_vector(DATA_WIDTH-1 downto 0);

	-- DECODER
	signal instr_word 							: std_logic_vector(DATA_WIDTH-1 downto 0) := x"00000013"; -- Initialize to NOP
	signal rs1_idx, rs2_idx, rd_idx 			: std_logic_vector(4 downto 0);
	signal opcode                   			: std_logic_vector(6 downto 0);
	signal funct3                   			: std_logic_vector(2 downto 0);
	signal funct7_5                 			: std_logic;
	signal imm_i, imm_s, imm_b, imm_u, imm_j 	: std_logic_vector(DATA_WIDTH-1 downto 0);
	-- Pipeline-aligned immediates for EXEC stage
	signal imm_i_ex, imm_s_ex, imm_u_ex 		: std_logic_vector(DATA_WIDTH-1 downto 0);
	
	-- ALU + BRANCH
	signal alu_src_b 				: std_logic_vector(DATA_WIDTH-1 downto 0);
	signal alu_result, alu_output 	: std_logic_vector(DATA_WIDTH-1 downto 0);
	signal alu_op_a, alu_op_b 		: std_logic_vector(DATA_WIDTH-1 downto 0); 	-- Registered ALU inputs
	signal alu_funct7_5_comb 		: std_logic := '0'; 						-- Combinational ALU operation selection
	signal alu_funct7_5 			: std_logic := '0'; 						-- Registered ALU operation selection
	signal alu_funct3 				: std_logic_vector(2 downto 0); 			-- Registered ALU function select
	signal branch_taken 			: std_logic;
	-- EXEC-stage registered control/operands for branches
	signal opcode_ex			: std_logic_vector(6 downto 0);
	signal rs1_ex, rs2_ex		: std_logic_vector(DATA_WIDTH-1 downto 0);
	
	-- CONTROL UNIT
	signal fetch1_en, fetch2_en, decode_en, reg_en, exec_en, mem_en, wb_en 	: std_logic;
	signal mem_read, mem_write    											: std_logic;
	signal pipe_flush 													    : std_logic; -- Pipeline flush signal
	-- WB-aligned destination index
	signal rd_idx_wb              											: std_logic_vector(4 downto 0);
	-- WB-aligned write enable
	signal reg_write_en_wb        											: std_logic;
	

begin
	-- Port mapping

	-- PC UNIT
	PCU: pc_unit
		generic map (DATA_WIDTH => DATA_WIDTH)
        port map (
            clk        		=> clk,
            rst_n      		=> rst_n,
            pc_sel     		=> pc_sel,
			pc_update_en 	=> pc_update_en,
            branch_off 		=> imm_b,
            jal_off    		=> imm_j,
            jalr_rs1   		=> rs1_val,
            jalr_off   		=> imm_i,
            pc_curr    		=> pc_curr
        );
		
	-- REGISTER FILE
	RF : register_file
        generic map (DATA_WIDTH => DATA_WIDTH, REG_COUNT => 32)
        port map (
            clk          => clk,
            rs1_idx      => rs1_idx,
            rs2_idx      => rs2_idx,
            rA_data      => rs1_val,
            rB_data      => rs2_val,
            rd_idx       => rd_idx_wb,
            rD_data      => writeback_data, -- Use WB mux (ALU or load)
            reg_write 	 => wb_en and reg_write_en_wb
        );

		
	-- DECODER
	DEC : decoder
        generic map (DATA_WIDTH => DATA_WIDTH)
        port map (
            clk          => clk,
            enable       => decode_en,
            instr        => instr_word,

            rs1_idx      => rs1_idx,
            rs2_idx      => rs2_idx,
            rd_idx       => rd_idx,

            imm_i        => imm_i,
            imm_s        => imm_s,
            imm_b        => imm_b,
            imm_u        => imm_u,
            imm_j        => imm_j,

            funct3       => funct3,
            funct7_5     => funct7_5,

            opcode       => opcode,
            reg_write_en => reg_write_en
        );
	
	-- ALU with stabilized inputs and outputs
	-- Calculate source operand B (either register or immediate)
	alu_src_b <= rs2_val when opcode = "0110011" else 
             (x"00000" & instr_word(31 downto 20)) when (opcode = "0010011" and (funct3 = "110" or funct3 = "111" or funct3 = "100")) else 
             imm_i;

	
	-- Prepare ALU operation selection (force ADD for I-type)
	-- For I-type shifts (funct3=101), imm[30] distinguishes SRLI (0) vs SRAI (1)
	-- For other I-type ops keep '0' as before; non-I types pass through funct7_5
	alu_funct7_5_comb <= '0' when (opcode = "0010011" and funct3 /= "101") else funct7_5;
	
	-- Register ALU inputs during REG_RD stage to stabilize values
	process(clk)
	begin
		if rising_edge(clk) then
			if rst_n = '0' then
				-- Synchronous reset of pipeline/control registers
				alu_op_a      <= (others => '0');
				alu_op_b      <= (others => '0');
				alu_funct3    <= (others => '0');
				alu_funct7_5  <= '0';
				opcode_ex     <= (others => '0');
				rs1_ex        <= (others => '0');
				rs2_ex        <= (others => '0');
				alu_result    <= (others => '0');
				rd_idx_wb     <= (others => '0');
				reg_write_en_wb <= '0';
				addr_mem        <= (others => '0');
				-- Reset pipeline-aligned immediates
				imm_i_ex        <= (others => '0');
				imm_s_ex        <= (others => '0');
				imm_u_ex        <= (others => '0');
			else
				-- Pipeline flush: clear pipeline registers when a branch is taken
				if pipe_flush = '1' then
					-- Clear ALU pipeline registers 
					alu_op_a <= (others => '0');
					alu_op_b <= (others => '0');
					alu_funct3 <= (others => '0');
					alu_funct7_5 <= '0';
					opcode_ex <= (others => '0');
					rs1_ex <= (others => '0');
					rs2_ex <= (others => '0');
					-- Don't clear ALU result as it may be needed for in-flight instructions
				elsif reg_en = '1' then
					-- Only update ALU input registers during register read stage
					alu_op_a <= rs1_val;
					alu_op_b <= alu_src_b;
					alu_funct3 <= funct3;
					alu_funct7_5 <= alu_funct7_5_comb;
					opcode_ex <= opcode;
					-- Latch destination register index for WB stage alignment
					rd_idx_wb <= rd_idx;
					-- Capture branch compare operands directly from RF (no WB->EXEC forwarding)
					rs1_ex <= rs1_val;
					rs2_ex <= rs2_val;
					reg_write_en_wb <= reg_write_en;
					-- Align immediates with EXEC stage
					imm_i_ex <= imm_i;
					imm_s_ex <= imm_s;
					imm_u_ex <= imm_u;
				end if;
				
				-- Latch effective address at end of EXEC stage for MEM access next cycle
				if exec_en = '1' then
					addr_mem <= addr_ex_comb;
				end if;
				
				-- ALU result register update not affected by pipeline flush
				if exec_en = '1' then
					-- Only update ALU result register during execution stage
					alu_result <= alu_output;
				end if;
			end if;
		end if;
	end process;

    -- Compute effective data address for LOAD/STORE in EXEC stage (combinational)
    -- Uses EXEC-stage aligned rs1_ex/opcode_ex and immediates
    process(all)
        variable base_u  : unsigned(DATA_WIDTH-1 downto 0);
        variable off_u   : unsigned(DATA_WIDTH-1 downto 0);
    begin
        base_u := unsigned(rs1_ex);
        -- Default offset: I-type immediate (also used by many I-type ALU ops)
        off_u  := unsigned(imm_i_ex);
        if opcode_ex = "0100011" then -- STORE
            off_u := unsigned(imm_s_ex);
        elsif opcode_ex = "0000011" then -- LOAD
            off_u := unsigned(imm_i_ex);
        end if;
        addr_ex_comb <= std_logic_vector(base_u + off_u);
    end process;

	-- RAM address select:
    --  - Use PC during instruction fetch (FETCH1/FETCH2)
    --  - Use addr_mem during data MEM accesses (load/store)
    -- mem_read is also asserted during FETCH1, so we must key off fetch enables, not mem_read.
    ram_addr <= pc_curr(ADDR_WIDTH+1 downto 2) when (fetch1_en = '1' or fetch2_en = '1') else
                addr_mem(ADDR_WIDTH+1 downto 2);
	
    ALU_P : alu
        generic map (DATA_WIDTH => DATA_WIDTH)
        port map (
            op_a     => alu_op_a,  		
            op_b     => alu_op_b,   	
            funct3   => alu_funct3, 	
            funct7_5 => alu_funct7_5,  -- Use registered control signal
            result   => alu_output
        );
		
	-- BRANCH COMPARE
	    BCMP : branch_cmp
        generic map (DATA_WIDTH => DATA_WIDTH)
        port map (
            rs1_val      => rs1_ex,
            rs2_val      => rs2_ex,
            funct3       => alu_funct3,
            branch_taken => branch_taken
        );
		
	-- Instruction register process with pipeline flush capability
	process(clk)
	begin
		if rising_edge(clk) then
			if pipe_flush = '1' then
				-- Clear instruction on pipeline flush (insert NOP - ADDI x0, x0, 0)
				instr_word <= x"00000013";
			elsif fetch2_en = '1' then 
				-- On branch flush, squash wrong-path instruction by injecting NOP
				if pipe_flush = '1' then
					instr_word <= x"00000013"; -- ADDI x0,x0,0 (NOP)
				else
					-- Normal instruction update
					instr_word <= ram_data;
				end if;
			end if;
		end if;
	end process;
	
	-- RAM
	-- Compute store byte enables in MEM stage based on funct3 and addr_mem(1:0)
	-- SW: 1111; SH: 0011 or 1100; SB: one-hot byte
	process(all)
	begin
		be_mem <= "0000"; -- default
		if opcode_ex = "0100011" then -- STORE
			case alu_funct3 is
				when "010" => -- SW
					be_mem <= "1111";
				when "001" => -- SH
					if addr_mem(1) = '0' then
						be_mem <= "0011"; -- low half
					else
						be_mem <= "1100"; -- high half
					end if;
				when "000" => -- SB
					case addr_mem(1 downto 0) is
						when "00" => be_mem <= "0001";
						when "01" => be_mem <= "0010";
						when "10" => be_mem <= "0100";
						when others => be_mem <= "1000";
					end case;
				when others =>
					be_mem <= "0000";
			end case;
		end if;
	end process;

	-- Load data sign/zero extension from RAM output using funct3 and addr_mem(1:0)
	process(all)
		variable byte_v  : std_logic_vector(7 downto 0);
		variable half_v  : std_logic_vector(15 downto 0);
	begin
		load_data_ext <= ram_data; -- default (LW)
		if opcode_ex = "0000011" then -- LOAD
			-- Select addressed byte and halfword
			case addr_mem(1 downto 0) is
				when "00" => byte_v := ram_data(7 downto 0);
				when "01" => byte_v := ram_data(15 downto 8);
				when "10" => byte_v := ram_data(23 downto 16);
				when others => byte_v := ram_data(31 downto 24);
			end case;
			if addr_mem(1) = '0' then
				half_v := ram_data(15 downto 0);
			else
				half_v := ram_data(31 downto 16);
			end if;
			case alu_funct3 is
				when "000" => -- LB (sign-extend 8-bit)
					load_data_ext <= std_logic_vector(resize(signed(byte_v), 32));
				when "100" => -- LBU (zero-extend 8-bit)
					load_data_ext <= std_logic_vector(resize(unsigned(byte_v), 32));
				when "001" => -- LH (sign-extend 16-bit)
					load_data_ext <= std_logic_vector(resize(signed(half_v), 32));
				when "101" => -- LHU (zero-extend 16-bit)
					load_data_ext <= std_logic_vector(resize(unsigned(half_v), 32));
				when others => -- LW or unsupported
					load_data_ext <= ram_data;
			end case;
		end if;
	end process;

	-- Align store data to selected lane
	process(all)
		variable byte_u : unsigned(31 downto 0);
		variable half_u : unsigned(31 downto 0);
	begin
		wr_data_mem <= rs2_val; -- default (SW)
		if opcode_ex = "0100011" then -- STORE
			case alu_funct3 is
				when "000" => -- SB: place rs2[7:0] into addressed byte lane
					byte_u := (others => '0');
					byte_u(7 downto 0) := unsigned(rs2_val(7 downto 0));
					case addr_mem(1 downto 0) is
						when "00" => wr_data_mem <= std_logic_vector(byte_u);
						when "01" => wr_data_mem <= std_logic_vector(shift_left(byte_u, 8));
						when "10" => wr_data_mem <= std_logic_vector(shift_left(byte_u, 16));
						when others => wr_data_mem <= std_logic_vector(shift_left(byte_u, 24));
					end case;
				when "001" => -- SH: place rs2[15:0] into addressed halfword (low/high)
					half_u := (others => '0');
					half_u(15 downto 0) := unsigned(rs2_val(15 downto 0));
					if addr_mem(1) = '0' then
						wr_data_mem <= std_logic_vector(half_u);
					else
						wr_data_mem <= std_logic_vector(shift_left(half_u, 16));
					end if;
				when others =>
					wr_data_mem <= rs2_val; -- SW
			end case;
		end if;
	end process;

    -- Write-back mux:
    --  - LUI    (0110111): write imm_u
    --  - AUIPC  (0010111): write pc_curr + imm_u
    --  - LOAD   (0000011): write load_data_ext
    --  - others: write ALU result
    writeback_data <= imm_u_ex                                                    when opcode_ex = "0110111" else
                      std_logic_vector(unsigned(pc_curr) + unsigned(imm_u_ex))    when opcode_ex = "0010111" else
                      load_data_ext                                               when opcode_ex = "0000011" else
                      alu_result;

	RAM_P : ram
        generic map (ADDR_WIDTH => ADDR_WIDTH, DATA_WIDTH => DATA_WIDTH, INIT_FILE  => INIT_FILE)
        port map (
            clk         => clk,
            addr        => ram_addr,
            wr_data     => wr_data_mem,
            byte_enable => be_mem,
            mem_read    => mem_read,
            mem_write   => mem_write,
            rd          => ram_data  
        );
	
	-- CONTROL UNIT
	CTRL : control_unit
        port map (
            clk          => clk,
            rst_n        => rst_n,
            opcode       => opcode_ex,
            funct3       => alu_funct3,
            branch_taken => branch_taken,
            
            -- Debug inputs
            pc_curr      => pc_curr,
            rs1_val      => rs1_val,
            rs2_val      => rs2_val,
            alu_result   => alu_result,
            rd_idx       => rd_idx,

            fetch1_en    => fetch1_en,
            fetch2_en    => fetch2_en,
            decode_en    => decode_en,
            reg_en       => reg_en,
            exec_en      => exec_en,
            mem_en       => mem_en,
            wb_en        => wb_en,

            pc_sel       => pc_sel,
            pc_update_en => pc_update_en,
            mem_read     => mem_read,
            mem_write    => mem_write,
            pipe_flush   => pipe_flush,
            trap         => trap_dbg
        );

    -- Debug outputs for TB logging
    wb_en_dbg        <= wb_en;
    reg_write_en_dbg <= reg_write_en_wb;
    rd_idx_dbg       <= rd_idx_wb;
    wb_data_dbg      <= writeback_data;

end rtl;