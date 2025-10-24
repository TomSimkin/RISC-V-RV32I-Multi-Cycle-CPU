library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity pc_unit is
	generic (
		DATA_WIDTH : positive := 32
	);
	port (
		clk				: in std_logic;
		rst_n 			: in std_logic; 								-- Active low reset
		pc_sel 			: in  std_logic_vector(1 downto 0);
		pc_update_en	: in  std_logic;                          		-- Enable signal for PC update
		branch_off   	: in  std_logic_vector(DATA_WIDTH-1 downto 0);  
        jal_off      	: in  std_logic_vector(DATA_WIDTH-1 downto 0);  
        jalr_rs1    	: in  std_logic_vector(DATA_WIDTH-1 downto 0);  
        jalr_off    	: in  std_logic_vector(DATA_WIDTH-1 downto 0);  
		-- Updated program counter
        pc_curr      	: out std_logic_vector(DATA_WIDTH-1 downto 0)
		
	);
end entity;

architecture rtl of pc_unit is
	signal pc_reg, pc_next : unsigned(DATA_WIDTH-1 downto 0);
	
begin
	-- Combinational logic, next-PC mux
	process(all)
	begin
		case pc_sel is 
			when "00" => 
				-- Sequential: PC + 4
				pc_next <= pc_reg + 4;

			when "01" =>
				-- Branch target: PC + sign_extend(B-imm)
				-- Use signed addition to handle negative offsets correctly
				pc_next <= unsigned(signed(std_logic_vector(pc_reg)) + signed(branch_off) - to_signed(4, DATA_WIDTH));

			when "10" =>
				-- JAL target: PC + sign_extend(J-imm)
				pc_next <= unsigned(signed(std_logic_vector(pc_reg)) + signed(jal_off) - to_signed(4, DATA_WIDTH));

			when others =>
				-- JALR target: (rs1 + sign_extend(I-imm)) & ~1
				pc_next <= (unsigned(jalr_rs1) + unsigned(jalr_off)) and x"FFFFFFFE"; -- Force LSB to be 0, address must be even
		end case;
	end process;
	
	-- PC register
	process(clk)
	begin
		if rising_edge(clk) then
			if rst_n = '0' then
				pc_reg <= (others => '0');
			else
				-- Only update PC when enabled
				if pc_update_en = '1' then
					pc_reg <= pc_next;
				end if;
			end if;
		end if;
	end process;
	
	pc_curr <= std_logic_vector(pc_reg);
	
end architecture;

