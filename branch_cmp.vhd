library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity branch_cmp is
	generic (
		DATA_WIDTH : positive := 32
	);
	port (
		rs1_val      : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        rs2_val      : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        funct3       : in  std_logic_vector(2 downto 0);  
        branch_taken : out std_logic                     		-- 1 = use branch
	);
end entity;

architecture rtl of branch_cmp is
	signal a_u, b_u : unsigned(DATA_WIDTH-1 downto 0);
    signal a_s, b_s : signed  (DATA_WIDTH-1 downto 0);

begin
	a_u <= unsigned (rs1_val);
	b_u <= unsigned (rs2_val);
	a_s <= signed 	(rs1_val);
	b_s <= signed 	(rs2_val);
	
	process(all)
	begin
		branch_taken <= '0'; -- Default
		
		case funct3 is 
			when "000" =>                   	-- BEQ
                if a_u = b_u then 
					branch_taken <= '1'; 
				end if;

            when "001" =>                   	-- BNE
                if a_u /= b_u then 
					branch_taken <= '1'; 
				end if;

            when "100" =>                   	-- BLT  (signed)
                if a_s <  b_s then 
					branch_taken <= '1'; 
				end if;
				
            when "101" =>                   	-- BGE  (signed)
                if a_s >= b_s then 
					branch_taken <= '1'; 
				end if;

            when "110" =>                     	-- BLTU (unsigned)
                if a_u <  b_u then 
					branch_taken <= '1'; 
				end if;

            when "111" =>                       -- BGEU (unsigned)
                if a_u >= b_u then 
					branch_taken <= '1'; 
				end if;

            when others => branch_taken <= '0';
        end case;
	end process;
	
end architecture;