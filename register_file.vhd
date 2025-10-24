library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity register_file is
	generic (
		DATA_WIDTH 	: positive := 32;	-- 32-bit data
		REG_COUNT	: positive := 32	-- 32 registers
	);
    port ( 
		clk 			: in  	std_logic;
		
		-- Read port A
		rs1_idx 		: in  	std_logic_vector(4 downto 0);
		rA_data 		: out  	std_logic_vector(DATA_WIDTH-1 downto 0);
		
		-- Read port B
		rs2_idx			: in  	std_logic_vector(4 downto 0);
		rB_data 		: out  	std_logic_vector(DATA_WIDTH-1 downto 0);
		
		-- Write port D
		rd_idx 			: in  	std_logic_vector(4 downto 0);
		rD_data 		: in  	std_logic_vector(DATA_WIDTH-1 downto 0);
		reg_write 		: in  	std_logic
	);
end entity;

architecture rtl of register_file is
	type reg_array is array(0 to REG_COUNT-1) of std_logic_vector(DATA_WIDTH-1 downto 0); -- 32x32 bit storage
	
	signal reg_file: reg_array := (others => (others => '0'));
begin
	-- Asynchronous reads --
	rA_data <= (others => '0') when rs1_idx = "00000"
				else reg_file(to_integer(unsigned(rs1_idx)));
				
	rB_data <= (others => '0') when rs2_idx = "00000"
				else reg_file(to_integer(unsigned(rs2_idx)));
				
	-- Synchronous write
	process(clk) 
	begin
		if rising_edge(clk) then
			if reg_write = '1' then
				if rd_idx /= "00000" then
					reg_file(to_integer(unsigned(rd_idx))) <= rD_data;
				end if;
			end if;
		end if;
	end process;
		
end architecture;

