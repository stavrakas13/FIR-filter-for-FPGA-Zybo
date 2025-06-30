library IEEE;
library work;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.math_real.all;

entity FIR is
       Port ( 
        clk: in std_logic;
        rst: in std_logic;
        valid_in : in std_logic;
        x: in std_logic_vector(7 downto 0);
        ram_out,rom_out: inout std_logic_vector(7 downto 0);
        y: out std_logic_vector(16  downto 0);
        valid_out : out std_logic;
        we, mac_init: inout std_logic;
        ram_address: inout std_logic_vector(2 downto 0)
        );
end FIR;

architecture Behavioral of FIR is    
    signal x_reg, rom_reg: std_logic_vector(7 downto 0);
    signal mac_init_reg, ram_we: std_logic;   
    signal rom_address: std_logic_vector(2 downto 0);
 --   signal ram_address: std_logic_vector(2 downto 0);
 --   signal mac_init: std_logic;
 --   signal we:std_logic;
 --   signal ram_out: std_logic_vector(7 downto 0);
 --   signal rom_out: std_logic_vector(7 downto 0);
       
begin

CU: entity work.control_unit port map(
    clk => clk,
    rst=>rst,
    rom_address =>rom_address,
    mac_init => mac_init,
    ram_address => ram_address,
    we => we
    );
    ram_we<= we and valid_in;


 
RAM: entity work.mlab_ram port map(
    clk  => clk,
    rst  => rst,
    we   => ram_we,                       
    en   => '1',               
    addr => ram_address,           
    di   => x,     
    do   => ram_out    
    );
    
ROM: entity work.mlab_rom port map(
    clk  => clk,
    en   => '1',              
    addr => rom_address,              
    rom_out =>  rom_out    
    );
    
MC: entity work.MAC port map(
    rom_out  => rom_out,
    ram_out  => ram_out,
    mac_init => mac_init_reg,
    L => y,
    clk => clk
   );
   
process(clk)
  begin
    if(rising_edge(clk)) then
--        x_reg <= x;
        mac_init_reg <= mac_init;
--        rom_reg<=rom_out;
    end if;
end process;

end Behavioral;