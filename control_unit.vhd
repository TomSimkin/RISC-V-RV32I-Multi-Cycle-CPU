library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity control_unit is
    port ( 
		clk 			: in  std_logic;
        rst_n 			: in  std_logic;
		
		-- Signals from decoder
		opcode 			: in std_logic_vector(6 downto 0);
		funct3			: in std_logic_vector(2 downto 0);
		
		-- Signal from branch 
		branch_taken 	: in std_logic;
		
		-- Debug inputs
		pc_curr			: in std_logic_vector(31 downto 0); -- Current PC value for debug
		rs1_val			: in std_logic_vector(31 downto 0); -- RS1 register value
		rs2_val			: in std_logic_vector(31 downto 0); -- RS2 register value
		alu_result		: in std_logic_vector(31 downto 0); -- ALU result
		rd_idx			: in std_logic_vector(4 downto 0);  -- Destination register index
		
		-- Global enables (one-hot encoding, one enable per pipeline stage)
		fetch1_en   	: out std_logic;
		fetch2_en   	: out std_logic;
        decode_en   	: out std_logic;
        reg_en      	: out std_logic;
        exec_en    		: out std_logic;
        mem_en      	: out std_logic;
        wb_en       	: out std_logic;
		
		-- Control to pc_unit
		pc_sel 			: out std_logic_vector(1 downto 0);
		pc_update_en	: out std_logic;
		
		-- RAM controls
		mem_read 		: out std_logic;
		mem_write		: out std_logic;
		
		-- Pipeline control
		pipe_flush      : out std_logic; -- Pipeline flush signal for branches
		
		-- Trap flag for ECALL/EBREAK
		trap 			: out std_logic
	);
end entity;

architecture rtl of control_unit is
	type state is (FETCH1, FETCH2, DECODE, REG_RD, EXEC, MEM, WB); 
	signal current_state : state := FETCH1;  -- Explicitly initialize to FETCH1
	signal next_state : state := FETCH1;     -- Explicitly initialize to FETCH1
	signal first_cycle : std_logic := '1'; -- Indicates first cycle after reset
    -- Synchronized branch-taken indicator (asserted exactly one cycle when a branch fires in EXEC)
    signal branch_sync : std_logic := '0';
	
	-- Helper functions to determine if opcode is load/store
	function is_load(op : std_logic_vector) return boolean is
	begin 
		return op = "0000011"; 
	end function;

	function is_store(op : std_logic_vector) return boolean is
	begin
		return op = "0100011";
	end function;

begin
	-- FSM 
	process(all)
	begin
		-- Force FETCH1 only during active reset, not during first cycle
		if rst_n = '0' then
			next_state <= FETCH1;
		else
			case current_state is
				when FETCH1 	=> next_state <= FETCH2;
				when FETCH2 	=> next_state <= DECODE;
				when DECODE 	=> next_state <= REG_RD;
				when REG_RD 	=> next_state <= EXEC;
				when EXEC 		=>
					if is_load(opcode) or is_store(opcode) then
						next_state <= MEM;
					else
						next_state <= WB;
					end if;
				when MEM 		=> next_state <= WB;
				when WB 		=> next_state <= FETCH1;
			end case;
		end if;
	end process;
	
	-- Current state and first_cycle update, and synchronous branch fire capture
    process(clk)
    begin
        if rising_edge(clk) then
            if rst_n = '0' then
                current_state <= FETCH1;
                first_cycle <= '1';
                branch_sync <= '0';
            else
                if first_cycle = '1' then
                    first_cycle <= '0';
                end if;

                current_state <= next_state;

                -- Latch branch exactly when in EXEC and it is taken 
                if (current_state = EXEC and opcode = "1100011" and branch_taken = '1') then
                    branch_sync <= '1';
                else
                    branch_sync <= '0';
                end if;
            end if;
        end if;
    end process;
	
	-- Stage enables (truly one-hot, each enable active for exactly one state)
	fetch1_en  <= '1' when current_state = FETCH1 else '0';
	fetch2_en  <= '1' when current_state = FETCH2 else '0';
    decode_en <= '1' when current_state = DECODE else '0';
    reg_en    <= '1' when current_state = REG_RD else '0';
    exec_en   <= '1' when current_state = EXEC   else '0';
    mem_en    <= '1' when current_state = MEM    else '0';
    wb_en     <= '1' when current_state = WB     else '0';
	
	-- Memory enables
	mem_read  <= '1' when (current_state = FETCH1 or (current_state = MEM and is_load(opcode))) else '0';
    mem_write <= '1' when (current_state = MEM and is_store(opcode)) else '0';
    
    -- PC update enable - update at WB, or immediately when a branch fires (branch_sync)
    pc_update_en <= '1' when (current_state = WB) or (branch_sync = '1') else '0';
	
	-- PC select logic (valid in both WB and during branch fire)
    process(all)
    begin
        pc_sel <= "00";                                   	-- Default SEQ

        if (current_state = WB) then
            -- Normal end-of-instruction PC update
            case opcode is
                when "1101111" => pc_sel <= "10";     	-- JAL
                when "1100111" => pc_sel <= "11";     	-- JALR
                when others    => pc_sel <= "00";    	-- SEQ
            end case;
        elsif (branch_sync = '1' and opcode = "1100011") then
            -- Early PC update for branch taken (synchronized)
            pc_sel <= "01";                               	-- Branch target
        end if;
    end process;
	
	-- Trap acknowledgement
	trap <= '1' when (current_state = EXEC and opcode = "1110011") else '0';
	
	-- Pipeline flush signal (active only when a branch fire is latched)
    pipe_flush <= branch_sync;
	
end architecture;
