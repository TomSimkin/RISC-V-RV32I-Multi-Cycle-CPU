library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_textio.all; 
use std.textio.all;
use std.env.all;  

entity cpu_top_tb is
end entity;

architecture sim of cpu_top_tb is
    -- DUT generics
    constant DATA_WIDTH : positive := 32;
    constant ADDR_WIDTH : positive := 8;
    constant INIT_FILE  : string   := "shift.mem"; -- Change name for different tests

    -- DUT ports
    signal clk   : std_logic := '0';
    signal rst_n : std_logic := '0';

    -- Simulation timing configuration
    constant CLK_PERIOD       : time    := 10 ns;
    -- Expected finals file for self-checking ("golden"). Change name for different tests.
    constant GOLDEN_FILE      : string  := "shift_golden.txt";

    -- Debug write-back taps from DUT
    signal wb_en_dbg        : std_logic;
    signal reg_write_en_dbg : std_logic;
    signal rd_idx_dbg       : std_logic_vector(4 downto 0);
    signal wb_data_dbg      : std_logic_vector(DATA_WIDTH-1 downto 0);
    -- Trap from DUT (EBREAK)
    signal trap_dbg         : std_logic := '0';

    -- Scoreboard: architectural register file mirror
    type reg_arr_t is array (0 to 31) of std_logic_vector(DATA_WIDTH-1 downto 0);
    signal rf_last : reg_arr_t := (others => (others => '0'));



begin
    -- Clock generation (10 ns period)
    clk <= not clk after CLK_PERIOD/2;

    -- Reset process 
    process
    begin
        rst_n <= '0';
        wait for 5*CLK_PERIOD;  
        rst_n <= '1';
        wait;
    end process;

    -- DUT instantiation
    DUT: entity work.cpu_top
        generic map (
            DATA_WIDTH => DATA_WIDTH,
            ADDR_WIDTH => ADDR_WIDTH,
            INIT_FILE  => INIT_FILE
        )
        port map (
            clk   => clk,
            rst_n => rst_n,
            wb_en_dbg        => wb_en_dbg,
            reg_write_en_dbg => reg_write_en_dbg,
            rd_idx_dbg       => rd_idx_dbg,
            wb_data_dbg      => wb_data_dbg,
            trap_dbg         => trap_dbg
        );
    
    -- Write-back monitor: captures every architectural register write (rd != x0)
    process(clk)
    begin
        if rising_edge(clk) then
            if wb_en_dbg = '1' and reg_write_en_dbg = '1' and rd_idx_dbg /= "00000" then
                rf_last(to_integer(unsigned(rd_idx_dbg))) <= wb_data_dbg;
            end if;
        end if;
    end process;

    -- Stop on TRAP, then self-check against GOLDEN_FILE
    process
        variable L          : line;
        variable exp_word   : std_logic_vector(DATA_WIDTH-1 downto 0);
        variable mismatches : integer := 0;
        file     expf       : text open read_mode is GOLDEN_FILE;
    begin
        -- Wait until TRAP is asserted (EBREAK)
        wait until rising_edge(clk) and trap_dbg = '1';
        report "TRAP observed: program signaled completion" severity note;

        -- Optional: one extra cycle for any final WB to settle
        wait until rising_edge(clk);

        -- Self-check: compare rf_last against GOLDEN_FILE
        for i in 0 to 31 loop
            if endfile(expf) then
                report "EXPECTED FILE ENDED EARLY at register index " & integer'image(i) severity error;
                exit;
            end if;
            readline(expf, L);
            hread(L, exp_word);
            if rf_last(i) /= exp_word then
                report "MISMATCH: x" & integer'image(i) & " got 0x" & to_hstring(rf_last(i)) &
                       " expected 0x" & to_hstring(exp_word) severity error;
                mismatches := mismatches + 1;
            end if;
        end loop;

        if mismatches = 0 then
            report "SCOREBOARD PASS: all 32 registers match expected" severity note;
        else
            report "SCOREBOARD FAIL: " & integer'image(mismatches) & " mismatches" severity error;
        end if;

        report "=== SIMULATION COMPLETE ===";
        stop;
        wait;
    end process;

end architecture;
