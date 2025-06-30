library IEEE;
library work;
use IEEE.STD_LOGIC_1164.ALL;

entity tb_fir is
end tb_fir;

architecture Behavioral of tb_fir is
    signal clk: std_logic;
    signal rst: std_logic;
    signal valid_in : std_logic;
    signal x,ram_out,rom_out: std_logic_vector(7 downto 0);
    signal y: std_logic_vector(16  downto 0);
    signal valid_out : std_logic;
    signal we,mac_init: std_logic;
    signal ram_address: std_logic_vector(2 downto 0);
    constant CLKP : time := 10 ns;
begin
clk_proc: 
   process
   begin
    clk <= '0';
    wait for CLKP/2;
    clk <= '1';
    wait for CLKP/2;
   end process;

UUT: entity work.FIR port map (
    clk,
    rst,
    valid_in,
    x,
    ram_out,
    rom_out,
    y,
    valid_out,
    we,
    mac_init,
    ram_address
);

tb: process
    begin
        rst <= '1';
                valid_in <= '1';
                x <= "00000001";
                wait for CLKP/4;
                rst<='0';
                wait for 3*CLKP/4;
                wait for 7*CLKP;
                valid_in <= '1';
                x <= "00000010";
                wait for CLKP;
                wait for 7*CLKP;
                valid_in <= '1';
                x <= "00000011";
                wait for CLKP;
                wait for 7*CLKP;
                valid_in <= '1';
                x <= "00000110";
                wait for CLKP;
                wait for 7*CLKP;
                valid_in <= '1';
                x <= "00000111";
                wait for CLKP;
                wait for 7*CLKP;
                valid_in <= '1';
                x <= "00000100";
                wait for CLKP;
                wait for 7*CLKP;
                valid_in <= '1';
                x <= "00000101";
                wait for CLKP;
                wait for 7*CLKP;
                valid_in <= '1';
                x <= "00000011";
                wait for CLKP;
                wait for 7*CLKP;
                valid_in <= '1';
                
                
                valid_in <= '1';
                x <= "00000000";
                wait for CLKP;
                wait for 7*CLKP;
 
                 valid_in <= '1';
                --x <= (others => '0');
                wait for CLKP;
               wait for 7*CLKP;

                valid_in <= '1';
               -- x <= (others => '0');
                wait for CLKP;
                wait for 7*CLKP;
              
                valid_in <= '1';
               -- x <= (others => '0');
                wait for CLKP;
                wait for 7*CLKP;               
                
                valid_in <= '1';
               -- x <= (others => '0');
                wait for CLKP;
                wait for 7*CLKP;
 
                 valid_in <= '1';
               -- x <= (others => '0');
                wait for CLKP;
                wait for 7*CLKP;

                valid_in <= '1';
                --x <= (others => '0');
                wait for CLKP;
                wait for 7*CLKP;
              
                valid_in <= '1';
               --x <= (others => '0');
                wait for CLKP;
                wait for 7*CLKP; 
            end process;
end Behavioral;
