library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity decoder is
	generic (
		DATA_WIDTH : positive := 32
	);
    port (
		clk 			: in std_logic;
		enable			: in std_logic;
		instr			: in std_logic_vector(31 downto 0);	-- RV32I instructions are always 32 bits
		
		-- Register indices
		rs1_idx			: out std_logic_vector(4 downto 0);
		rs2_idx			: out std_logic_vector(4 downto 0);
		rd_idx			: out std_logic_vector(4 downto 0);
		
		-- Immediates (sign-extended to DATA_WIDTH)
		imm_i			: out std_logic_vector(DATA_WIDTH-1 downto 0);	-- I-type
		imm_s			: out std_logic_vector(DATA_WIDTH-1 downto 0);	-- S-type
		imm_b			: out std_logic_vector(DATA_WIDTH-1 downto 0);	-- B-type
		imm_u			: out std_logic_vector(DATA_WIDTH-1 downto 0);	-- U-type
		imm_j			: out std_logic_vector(DATA_WIDTH-1 downto 0);	-- J-type
		
		-- ALU control bits
		funct3			: out std_logic_vector(2 downto 0);
		funct7_5 		: out std_logic; -- Bit 30 of the instruction
		
		-- Basic control
		opcode			: out std_logic_vector(6 downto 0);
		reg_write_en	: out std_logic
	);
end entity;

architecture rtl of decoder is
begin
	-- Sequential process for control signals
	process(clk)
	begin
		if rising_edge(clk) then
			if enable = '1' then			
				-- Extract instruction to proper fields
				opcode      <= instr(6 downto 0);
				rd_idx      <= instr(11 downto 7);
				funct3      <= instr(14 downto 12);
	            rs1_idx     <= instr(19 downto 15);
	            rs2_idx     <= instr(24 downto 20);
	            funct7_5    <= instr(30);
	            
	            -- Generate reg_write_en synchronously to align with pipeline timing
	            case instr(6 downto 0) is -- Use direct instruction bits for immediate effect
	                when "0110011" | "0010011" | "0000011" | "1101111" | "0010111" | "0110111" =>
	                    reg_write_en <= '1';
	                when others =>
	                    reg_write_en <= '0';
	            end case;
	        end if;
	    end if;
	end process;
	
	-- Combinational process for immediate extraction
	process(instr)
	begin
		-- I-type immediate: bits [31:20], sign-extend
		imm_i <= std_logic_vector(resize(signed(instr(31 downto 20)), DATA_WIDTH));

		-- S-type immediate: bits [31:25] & [11:7], sign-extend
		imm_s <= std_logic_vector(resize(signed(instr(31 downto 25) & instr(11 downto 7)), DATA_WIDTH));

		-- B-type immediate: {imm[12]=instr[31], imm[11]=instr[7], imm[10:5]=instr[30:25], imm[4:1]=instr[11:8], imm[0]=0}, sign-extend
		imm_b <= std_logic_vector(resize(signed(instr(31) & instr(7) & instr(30 downto 25) & instr(11 downto 8) & '0'), DATA_WIDTH));

		-- U-type immediate: upper 20 bits << 12, zero-extend
		imm_u <= instr(31 downto 12) & x"000";

		-- J-type immediate: {imm[20]=instr[31], imm[19:12]=instr[19:12], imm[11]=instr[20], imm[10:1]=instr[30:21], imm[0]=0}, sign-extend
		imm_j <= std_logic_vector(resize(signed(instr(31) & instr(19 downto 12) & instr(20) & instr(30 downto 21) & '0'), DATA_WIDTH));
	end process;
	
end architecture;
