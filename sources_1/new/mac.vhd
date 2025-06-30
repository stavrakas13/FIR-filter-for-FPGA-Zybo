library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.math_real.all;

entity MAC is
   
    Port (
        rom_out: in std_logic_vector(7 downto 0);
        ram_out: in std_logic_vector(7 downto 0);
        mac_init : in std_logic;
        L: out std_logic_vector(16  downto 0);
        clk : in std_logic
    );
end MAC;

architecture Behavioral of MAC is
        signal temp : std_logic_vector(16 downto 0) ;   
        signal count: std_logic_vector(2 downto 0);            
begin
        process(clk)
            begin 
                 if (rising_edge(clk)) then
                        if mac_init = '1' then
                            temp <= (others => '0');
                            count <= "000";
                            temp(15 downto 0) <= ram_out * rom_out;
                        else 
                            temp <= temp + (ram_out * rom_out);
                            count <= count + '1'; 
                        end if;
                        if count = "111" then
                            L <= temp;
                        end if;
                 end if;
        end process;
end Behavioral;