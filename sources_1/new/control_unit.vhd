library IEEE;
library work;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.std_logic_unsigned.all;
use IEEE.NUMERIC_STD.ALL;

entity control_unit is
           port (
           clk, rst : in std_logic;
           rom_address : out std_logic_vector(2 downto 0);
           mac_init : out std_logic;
           ram_address : out std_logic_vector(2 downto 0);
           we : out std_logic
--           valid_out: out std_logic
           ); 
end control_unit;

architecture Behavioral of control_unit is
    signal count, sum: std_logic_vector(2 downto 0);
    signal cout: std_logic;
begin
 process(clk, rst)
    begin
        if rst = '1' then
            count <= (others => '0');
        elsif clk'event and clk = '1' then
            count <= count + 1;
            if count ="000" then
                cout<='1';
                we<='1';
                mac_init<='1';
            else
                cout<='0';
                we<='0';
                mac_init<='0';
            end if;
        end if;
    sum <= count;
            
        
    rom_address<=sum;
    ram_address<=sum;
    
    
    end process;

    
    
end Behavioral;
