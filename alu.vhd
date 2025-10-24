library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity alu is
	generic (
		DATA_WIDTH : positive := 32
	);
    port ( 
		-- Operands
		op_a 		: in std_logic_vector(DATA_WIDTH-1 downto 0);
		op_b 		: in std_logic_vector(DATA_WIDTH-1 downto 0);
		funct3		: in std_logic_vector(2 downto 0);
		funct7_5 	: in std_logic; -- Bit 30 of instruction (funct7[5]) or imm[30] for I-shifts
		result		: out std_logic_vector(DATA_WIDTH-1 downto 0)
	);
end entity;

architecture rtl of alu is
	-- Unsigned & signed versions of the operands
	signal a_u, b_u : unsigned(DATA_WIDTH-1 downto 0);
	signal a_s, b_s : signed(DATA_WIDTH-1 downto 0);
	
	-- Internal result
	signal res 		: std_logic_vector(DATA_WIDTH-1 downto 0);
	
	-- Shift amount (max 5 bits)
	signal shamt	: natural range 0 to DATA_WIDTH-1;
	
	begin
		a_u 	<= unsigned(op_a);	
		b_u 	<= unsigned(op_b);
		a_s 	<= signed(op_a);	
		b_s 	<= signed(op_b);
		shamt 	<= to_integer(b_u(4 downto 0));	-- Shift amount
		
		process(all)
		begin
			res <= (others => '0');				-- Default
			
			case funct3 is				
				when "000" => 					-- ADD/SUB (R-type) or ADDI behavior when funct7_5='0'
					if funct7_5 = '0' then		-- ADD / ADDI
						res <= std_logic_vector(a_s + b_s);
					else						-- SUB (R-type only)
						res <= std_logic_vector(a_s - b_s);
					end if;
				
				when "001" =>					-- SLL / SLLI (logical left)
					res <= std_logic_vector(shift_left(a_u, shamt));
					
				when "010" =>					-- SLT (signed)
					res <= (others => '0');
					if a_s < b_s then
						res(0) <= '1';
					end if;
				
				when "011" =>					-- SLTU (unsigned)
					res <= (others => '0');
					if a_u < b_u then
						res(0) <= '1';
					end if;
				
				when "100" =>					-- XOR / XORI
					res <= op_a xor op_b;
					
				when "101" =>					-- SRL/SRA and SRLI/SRAI (bit30 selects)
					if funct7_5 = '0' then		-- SRL / SRLI
						res <= std_logic_vector(shift_right(a_u, shamt));
					else						-- SRA / SRAI
						res <= std_logic_vector(shift_right(a_s, shamt));
					end if;
				
				when "110" =>					-- OR / ORI
					res <= op_a or op_b;
				
				when others =>				-- AND / ANDI
					res <= op_a and op_b;
			end case;
		end process;
		
		result <= res;
		
end rtl;
