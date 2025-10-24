library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_textio.all;
use std.textio.all;

entity ram is
    generic (
        ADDR_WIDTH : positive := 8;			-- 256 words, multiply by DATA_WIDTH and will get 1 KiB (kibibyte)
        DATA_WIDTH : positive := 32;
		INIT_FILE  : string	  := "RAM.mem"
    );
    port (
        clk         : in  std_logic;
        -- CPU interface (synchronous read/write)
        addr        : in  std_logic_vector(ADDR_WIDTH-1 downto 0); 
        wr_data     : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        byte_enable : in  std_logic_vector(3 downto 0);            -- '1111'=full 32-bit write (SW), '0011'/'1100'=half word (SH), '0001,0010,0100,1000'=single byte (SB)
        mem_read    : in  std_logic;      
        mem_write   : in  std_logic;      
        rd       	: out std_logic_vector(DATA_WIDTH-1 downto 0)
    );
end entity;

architecture rtl of ram is
    type ram_array is array (0 to 2**ADDR_WIDTH-1) of std_logic_vector(DATA_WIDTH-1 downto 0);
    
    -- Impure function to initialize RAM from file 
    impure function init_ram_from_file return ram_array is
        file mem_file : text open read_mode is INIT_FILE;
        variable line_in : line;
        variable ram_content : ram_array;
        variable addr : integer := 0;
        variable slv_val : std_logic_vector(DATA_WIDTH-1 downto 0);
    begin
        -- RAM reset
        for i in 0 to 2**ADDR_WIDTH-1 loop
            ram_content(i) := (others => '0');
        end loop;
        
        -- Load .mem file
        while not endfile(mem_file) and addr < 2**ADDR_WIDTH loop
            readline(mem_file, line_in);
            
            -- Skip empty lines
            if line_in'length > 0 then
                hread(line_in, slv_val);
                ram_content(addr) := slv_val;
                addr := addr + 1;
            end if;
        end loop;

        return ram_content;
    end function;

    -- RAM signal declaration with initialization from file
    signal ram : ram_array := init_ram_from_file;
    signal read_word : std_logic_vector(DATA_WIDTH-1 downto 0) := (others => '0'); -- Use internal signal to guarantee correct timing
    
begin
	
	-- Synchronous write + read
	process(clk)
	begin
		if rising_edge(clk) then
			if mem_write = '1' then
				for i in 0 to 3 loop	-- Loop over the four bytes that make up one 32-bit word
					if byte_enable(i) = '1' then
						ram(to_integer(unsigned(addr)))(8*i+7 downto 8*i) -- Overwrite the data in each address byte 
							<= wr_data(8*i+7 downto 8*i);
					end if;
				end loop;
			end if;
			
			-- Read
			if mem_read = '1' then
				read_word <= ram(to_integer(unsigned(addr)));
			end if;
		end if;
	end process;
	
	rd <= read_word;
	
end architecture;