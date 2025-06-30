library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.std_logic_unsigned.all;
use IEEE.NUMERIC_STD.ALL;

entity control_unit is
    port (
        clk_u, rst_u: in std_logic;
        valid_in_u: in std_logic;
        rom_address_u: out std_logic_vector(2 downto 0);
        mac_init_u: out std_logic;
        ram_address_u: out std_logic_vector(2 downto 0);
        we_u: out std_logic
    ); 
end control_unit;

architecture Behavioral of control_unit is
    signal count_u, sum_u: std_logic_vector(3 downto 0);
begin
    process(clk_u, rst_u)
    begin
        if rst_u = '1' then
            count_u <= (others => '0');
        elsif rising_edge(clk_u) then
            if valid_in_u = '1' then
                count_u <= "0000";
                we_u <= '1';
                mac_init_u <= '1';
                --valid_in_u <= '0';
            else
                if count_u > "0111" then
                    count_u <= count_u;
                else
                    count_u <= count_u + 1;
                end if;
                we_u <= '0';
                mac_init_u <= '0';
            end if;
        end if;
        sum_u <= count_u;
        rom_address_u <= sum_u(2 downto 0);
        ram_address_u <= sum_u(2 downto 0);
    end process;
end Behavioral;


library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity mlab_ram is
     generic (
        data_width : integer := 8  -- width of data (bits)
     );
    port (
        clk_ram: in std_logic;
        rst_ram: in std_logic;
        we_ram: in std_logic;      -- memory write enable
        en_ram: in std_logic;      -- operation enable
        addr_ram: in std_logic_vector(2 downto 0);  -- memory address
        di_ram: in std_logic_vector(data_width - 1 downto 0);  -- input data
        do_ram: out std_logic_vector(data_width - 1 downto 0)  -- output data
    );
end mlab_ram;

architecture mram of mlab_ram is
    type ram_type is array (7 downto 0) of std_logic_vector (data_width - 1 downto 0);
    signal RAM: ram_type := (others => (others => '0'));

begin
    process (clk_ram, rst_ram)
    begin
        if rst_ram = '1' then
            RAM <= (others => (others => '0'));
        else
            if rising_edge(clk_ram) then
                if en_ram = '1' then
                    if we_ram = '1' then
                        RAM(7) <= RAM(6);  -- write operation
                        RAM(6) <= RAM(5);
                        RAM(5) <= RAM(4);
                        RAM(4) <= RAM(3);
                        RAM(3) <= RAM(2);
                        RAM(2) <= RAM(1);
                        RAM(1) <= RAM(0);
                        RAM(0) <= di_ram;
                        do_ram <= di_ram;
                    else
                        do_ram <= RAM(conv_integer(addr_ram));
                    end if;
                end if;
            end if;
        end if;
    end process;
end mram;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.std_logic_unsigned.all;

entity mlab_rom is
    generic (
        coeff_width : integer := 8  -- width of coefficients (bits)
    );
    Port (
        clk_rom: in  STD_LOGIC;
        en_rom: in  STD_LOGIC;  -- operation enable
        addr_rom: in  STD_LOGIC_VECTOR (2 downto 0);  -- memory address
        rom_out_rom: out  STD_LOGIC_VECTOR (coeff_width - 1 downto 0)  -- output data
    );
end mlab_rom;

architecture Behavioral of mlab_rom is

    type rom_type is array (7 downto 0) of std_logic_vector (coeff_width - 1 downto 0);
    signal rom: rom_type := (
        "00001000", "00000111", "00000110", "00000101", "00000100", "00000011", "00000010", "00000001"
    );  -- initialization of rom with user data

    signal rdata: std_logic_vector(coeff_width - 1 downto 0) := (others => '0');
begin

    rdata <= rom(conv_integer(addr_rom));

    process (clk_rom)
    begin
        if rising_edge(clk_rom) then
            if (en_rom = '1') then
                rom_out_rom <= rdata;
            end if;
        end if;
    end process;

end Behavioral;


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.math_real.all;

entity MAC is
    Port (
        rom_out_mac: in std_logic_vector(7 downto 0);
        ram_out_mac: in std_logic_vector(7 downto 0);
        mac_init_mac: in std_logic;
        L_mac: out std_logic_vector(16 downto 0);
        valid_out_mac: out std_logic;
        clk_mac: in std_logic;
        temp_mac: inout std_logic_vector(16 downto 0)
    );
end MAC;

architecture Behavioral of MAC is
    signal count_mac: std_logic_vector(3 downto 0);
    signal sig: std_logic;
begin
    process(clk_mac)
    begin 
        if rising_edge(clk_mac) then
            if mac_init_mac = '1' then
                temp_mac <= (others => '0');
                count_mac <= "0000";
                temp_mac(15 downto 0) <= ram_out_mac * rom_out_mac;
                valid_out_mac <= '0';
            else 
                if rom_out_mac = "00000001" then
                    temp_mac <= (others => '0');
                    count_mac <= "0000";
                    temp_mac(15 downto 0) <= ram_out_mac * rom_out_mac;
                    valid_out_mac <= '0';
                else 
                    temp_mac <= temp_mac + (ram_out_mac * rom_out_mac);
                    count_mac <= count_mac + '1'; 
                    valid_out_mac <= '0'; 
                end if;
            end if;
            if rom_out_mac = "00001000" then
                sig <= '1';
            elsif rom_out_mac = "00000001" then
                if sig = '1' then
                    L_mac <= temp_mac;
                    valid_out_mac <= '1';
                    sig <= '0';
                end if;
            end if;
        end if;
    end process;
end Behavioral;


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
        ram_address: inout std_logic_vector(2 downto 0);
        temp1:inout std_logic_vector(16 downto 0)
        );
end FIR;

architecture Behavioral of FIR is    
    signal x_reg, rom_reg: std_logic_vector(7 downto 0);
    signal mac_init_reg, ram_we: std_logic;   
    signal rom_address: std_logic_vector(2 downto 0);
    signal writed: std_logic;
 --   signal ram_address: std_logic_vector(2 downto 0);
 --   signal mac_init: std_logic;
 --   signal we:std_logic;
 --   signal ram_out: std_logic_vector(7 downto 0);
 --   signal rom_out: std_logic_vector(7 downto 0);
component control_unit is
    port (
        clk_u          : in  std_logic;
        rst_u          : in  std_logic;
        valid_in_u     : in  std_logic;
        rom_address_u  : out std_logic_vector(2 downto 0);
        mac_init_u     : out std_logic;
        ram_address_u  : out std_logic_vector(2 downto 0);
        we_u           : out std_logic
    ); 
end component control_unit;
component MAC is
    port (
        rom_out_mac    : in  std_logic_vector(7 downto 0);
        ram_out_mac    : in  std_logic_vector(7 downto 0);
        mac_init_mac   : in  std_logic;
        L_mac          : out std_logic_vector(16 downto 0);
        valid_out_mac  : out std_logic;
        clk_mac        : in  std_logic;
        temp_mac       : inout std_logic_vector(16 downto 0)
    );
end component MAC;

component mlab_rom is
    generic (
        coeff_width : integer := 8  -- width of coefficients (bits)
    );
    port (
        clk_rom      : in  std_logic;
        en_rom       : in  std_logic;                        -- operation enable
        addr_rom     : in  std_logic_vector(2 downto 0);     -- memory address
        rom_out_rom  : out std_logic_vector(coeff_width-1 downto 0) -- output data
    );
end component mlab_rom;

component mlab_ram is
    generic (
        data_width : integer := 8  -- width of data (bits)
    );
    port (
        clk_ram  : in  std_logic;
        rst_ram  : in  std_logic;
        we_ram   : in  std_logic;                        -- memory write enable
        en_ram   : in  std_logic;                        -- operation enable
        addr_ram : in  std_logic_vector(2 downto 0);     -- memory address
        di_ram   : in  std_logic_vector(data_width-1 downto 0); -- input data
        do_ram   : out std_logic_vector(data_width-1 downto 0)  -- output data
    );
end component mlab_ram;

begin

CU: control_unit port map(
    clk_u => clk,
    rst_u=>rst,
    valid_in_u => valid_in,
    rom_address_u =>rom_address,
    mac_init_u => mac_init,
    ram_address_u => ram_address,
    we_u => we
    );

 
RAM: mlab_ram port map(
    clk_ram  => clk,
    rst_ram  => rst,
    we_ram   => we,                       
    en_ram   => '1',               
    addr_ram => ram_address,           
    di_ram   => x,     
    do_ram   => ram_out
    );
    
ROM: mlab_rom port map(
    clk_rom  => clk,
    en_rom   => '1',              
    addr_rom => rom_address,              
    rom_out_rom =>  rom_out    
    );
    
MC: MAC port map(
    rom_out_mac  => rom_out,
    ram_out_mac  => ram_out,
    mac_init_mac => mac_init_reg,
    L_mac => y,
    valid_out_mac => valid_out,
    clk_mac => clk,
    temp_mac => temp1
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


library ieee;
library work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity final_Axi4lite_v1_0_S00_AXI is
	generic (
		-- Users to add parameters here

		-- User parameters ends
		-- Do not modify the parameters beyond this line

		-- Width of S_AXI data bus
		C_S_AXI_DATA_WIDTH	: integer	:= 32;
		-- Width of S_AXI address bus
		C_S_AXI_ADDR_WIDTH	: integer	:= 4
	);
	port (
		-- Users to add ports here
        signal 
		-- User ports ends
		-- Do not modify the ports beyond this line

		-- Global Clock Signal
		S_AXI_ACLK	: in std_logic;
		-- Global Reset Signal. This Signal is Active LOW
		S_AXI_ARESETN	: in std_logic;
		-- Write address (issued by master, acceped by Slave)
		S_AXI_AWADDR	: in std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
		-- Write channel Protection type. This signal indicates the
    		-- privilege and security level of the transaction, and whether
    		-- the transaction is a data access or an instruction access.
		S_AXI_AWPROT	: in std_logic_vector(2 downto 0);
		-- Write address valid. This signal indicates that the master signaling
    		-- valid write address and control information.
		S_AXI_AWVALID	: in std_logic;
		-- Write address ready. This signal indicates that the slave is ready
    		-- to accept an address and associated control signals.
		S_AXI_AWREADY	: out std_logic;
		-- Write data (issued by master, acceped by Slave) 
		S_AXI_WDATA	: in std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
		-- Write strobes. This signal indicates which byte lanes hold
    		-- valid data. There is one write strobe bit for each eight
    		-- bits of the write data bus.    
		S_AXI_WSTRB	: in std_logic_vector((C_S_AXI_DATA_WIDTH/8)-1 downto 0);
		-- Write valid. This signal indicates that valid write
    		-- data and strobes are available.
		S_AXI_WVALID	: in std_logic;
		-- Write ready. This signal indicates that the slave
    		-- can accept the write data.
		S_AXI_WREADY	: out std_logic;
		-- Write response. This signal indicates the status
    		-- of the write transaction.
		S_AXI_BRESP	: out std_logic_vector(1 downto 0);
		-- Write response valid. This signal indicates that the channel
    		-- is signaling a valid write response.
		S_AXI_BVALID	: out std_logic;
		-- Response ready. This signal indicates that the master
    		-- can accept a write response.
		S_AXI_BREADY	: in std_logic;
		-- Read address (issued by master, acceped by Slave)
		S_AXI_ARADDR	: in std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
		-- Protection type. This signal indicates the privilege
    		-- and security level of the transaction, and whether the
    		-- transaction is a data access or an instruction access.
		S_AXI_ARPROT	: in std_logic_vector(2 downto 0);
		-- Read address valid. This signal indicates that the channel
    		-- is signaling valid read address and control information.
		S_AXI_ARVALID	: in std_logic;
		-- Read address ready. This signal indicates that the slave is
    		-- ready to accept an address and associated control signals.
		S_AXI_ARREADY	: out std_logic;
		-- Read data (issued by slave)
		S_AXI_RDATA	: out std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
		-- Read response. This signal indicates the status of the
    		-- read transfer.
		S_AXI_RRESP	: out std_logic_vector(1 downto 0);
		-- Read valid. This signal indicates that the channel is
    		-- signaling the required read data.
		S_AXI_RVALID	: out std_logic;
		-- Read ready. This signal indicates that the master can
    		-- accept the read data and response information.
		S_AXI_RREADY	: in std_logic
	);
end final_Axi4lite_v1_0_S00_AXI;

architecture arch_imp of final_Axi4lite_v1_0_S00_AXI is

	-- AXI4LITE signals
	signal axi_awaddr	: std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
	signal axi_awready	: std_logic;
	signal axi_wready	: std_logic;
	signal axi_bresp	: std_logic_vector(1 downto 0);
	signal axi_bvalid	: std_logic;
	signal axi_araddr	: std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
	signal axi_arready	: std_logic;
	signal axi_rdata	: std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
	signal axi_rresp	: std_logic_vector(1 downto 0);
	signal axi_rvalid	: std_logic;

	-- Example-specific design signals
	-- local parameter for addressing 32 bit / 64 bit C_S_AXI_DATA_WIDTH
	-- ADDR_LSB is used for addressing 32/64 bit registers/memories
	-- ADDR_LSB = 2 for 32 bits (n downto 2)
	-- ADDR_LSB = 3 for 64 bits (n downto 3)
	constant ADDR_LSB  : integer := (C_S_AXI_DATA_WIDTH/32)+ 1;
	constant OPT_MEM_ADDR_BITS : integer := 1;
	------------------------------------------------
	---- Signals for user logic register space example
	signal A_ip, B_ip: std_logic_vector(31 downto 0);
	signal x_ip, rom_out_ip, ram_out_ip: std_logic_vector(7 downto 0);
	signal clk_ip, rst_ip, valid_in_ip, valid_out_ip, we_ip, mac_init_ip: std_logic;
	signal y_ip:std_logic_vector(16 downto 0);
	signal ram_address_ip: std_logic_vector(2 downto 0);
    

    component FIR is
    Port ( 
        clk         : in  std_logic;
        rst         : in  std_logic;
        valid_in    : in  std_logic;
        x           : in  std_logic_vector(7 downto 0);
        ram_out     : inout std_logic_vector(7 downto 0);
        rom_out     : inout std_logic_vector(7 downto 0);
        y           : out std_logic_vector(16 downto 0);
        valid_out   : out std_logic;
        we          : inout std_logic;
        mac_init    : inout std_logic;
        ram_address : inout std_logic_vector(2 downto 0);
        temp1       : inout std_logic_vector(16 downto 0)
    );
end component FIR;
----------------------------------------------
	---- Number of Slave Registers 4
	signal slv_reg0	:std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
	signal slv_reg1	:std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
	signal slv_reg2	:std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
	signal slv_reg3	:std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
	signal slv_reg_rden	: std_logic;
	signal slv_reg_wren	: std_logic;
	signal reg_data_out	:std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
	signal byte_index	: integer;
	signal aw_en	: std_logic;

begin
	-- I/O Connections assignments

	S_AXI_AWREADY	<= axi_awready;
	S_AXI_WREADY	<= axi_wready;
	S_AXI_BRESP	<= axi_bresp;
	S_AXI_BVALID	<= axi_bvalid;
	S_AXI_ARREADY	<= axi_arready;
	S_AXI_RDATA	<= axi_rdata;
	S_AXI_RRESP	<= axi_rresp;
	S_AXI_RVALID	<= axi_rvalid;
	-- Implement axi_awready generation
	-- axi_awready is asserted for one S_AXI_ACLK clock cycle when both
	-- S_AXI_AWVALID and S_AXI_WVALID are asserted. axi_awready is
	-- de-asserted when reset is low.

	process (S_AXI_ACLK)
	begin
	  if rising_edge(S_AXI_ACLK) then 
	    if S_AXI_ARESETN = '0' then
	      axi_awready <= '0';
	      aw_en <= '1';
	    else
	      if (axi_awready = '0' and S_AXI_AWVALID = '1' and S_AXI_WVALID = '1' and aw_en = '1') then
	        -- slave is ready to accept write address when
	        -- there is a valid write address and write data
	        -- on the write address and data bus. This design 
	        -- expects no outstanding transactions. 
	           axi_awready <= '1';
	           aw_en <= '0';
	        elsif (S_AXI_BREADY = '1' and axi_bvalid = '1') then
	           aw_en <= '1';
	           axi_awready <= '0';
	      else
	        axi_awready <= '0';
	      end if;
	    end if;
	  end if;
	end process;

	-- Implement axi_awaddr latching
	-- This process is used to latch the address when both 
	-- S_AXI_AWVALID and S_AXI_WVALID are valid. 

	process (S_AXI_ACLK)
	begin
	  if rising_edge(S_AXI_ACLK) then 
	    if S_AXI_ARESETN = '0' then
	      axi_awaddr <= (others => '0');
	    else
	      if (axi_awready = '0' and S_AXI_AWVALID = '1' and S_AXI_WVALID = '1' and aw_en = '1') then
	        -- Write Address latching
	        axi_awaddr <= S_AXI_AWADDR;
	      end if;
	    end if;
	  end if;                   
	end process; 

	-- Implement axi_wready generation
	-- axi_wready is asserted for one S_AXI_ACLK clock cycle when both
	-- S_AXI_AWVALID and S_AXI_WVALID are asserted. axi_wready is 
	-- de-asserted when reset is low. 

	process (S_AXI_ACLK)
	begin
	  if rising_edge(S_AXI_ACLK) then 
	    if S_AXI_ARESETN = '0' then
	      axi_wready <= '0';
	    else
	      if (axi_wready = '0' and S_AXI_WVALID = '1' and S_AXI_AWVALID = '1' and aw_en = '1') then
	          -- slave is ready to accept write data when 
	          -- there is a valid write address and write data
	          -- on the write address and data bus. This design 
	          -- expects no outstanding transactions.           
	          axi_wready <= '1';
	      else
	        axi_wready <= '0';
	      end if;
	    end if;
	  end if;
	end process; 

	-- Implement memory mapped register select and write logic generation
	-- The write data is accepted and written to memory mapped registers when
	-- axi_awready, S_AXI_WVALID, axi_wready and S_AXI_WVALID are asserted. Write strobes are used to
	-- select byte enables of slave registers while writing.
	-- These registers are cleared when reset (active low) is applied.
	-- Slave register write enable is asserted when valid address and data are available
	-- and the slave is ready to accept the write address and write data.
	slv_reg_wren <= axi_wready and S_AXI_WVALID and axi_awready and S_AXI_AWVALID ;

	process (S_AXI_ACLK)
	variable loc_addr :std_logic_vector(OPT_MEM_ADDR_BITS downto 0); 
	begin
	  if rising_edge(S_AXI_ACLK) then 
	    if S_AXI_ARESETN = '0' then
	      slv_reg0 <= (others => '0');
	    --   slv_reg1 <= (others => '0');
	      slv_reg2 <= (others => '0');
	      slv_reg3 <= (others => '0');
	    else
	      loc_addr := axi_awaddr(ADDR_LSB + OPT_MEM_ADDR_BITS downto ADDR_LSB);
	      if (slv_reg_wren = '1') then
	        case loc_addr is
	          when b"00" =>
	            for byte_index in 0 to (C_S_AXI_DATA_WIDTH/8-1) loop
	              if ( S_AXI_WSTRB(byte_index) = '1' ) then
	                -- Respective byte enables are asserted as per write strobes                   
	                -- slave registor 0
	                slv_reg0(byte_index*8+7 downto byte_index*8) <= S_AXI_WDATA(byte_index*8+7 downto byte_index*8);
	              end if;
	            end loop;
	        --  when b"01" => slv_reg2 <=(others => '0');
	            -- for byte_index in 0 to (C_S_AXI_DATA_WIDTH/8-1) loop
	            --   if ( S_AXI_WSTRB(byte_index) = '1' ) then
	            --     -- Respective byte enables are asserted as per write strobes                   
	            --     -- slave registor 1
	            --     slv_reg1(byte_index*8+7 downto byte_index*8) <= S_AXI_WDATA(byte_index*8+7 downto byte_index*8);
	            --   end if;
	            -- end loop;
	          when b"10" =>
	            for byte_index in 0 to (C_S_AXI_DATA_WIDTH/8-1) loop
	              if ( S_AXI_WSTRB(byte_index) = '1' ) then
	                -- Respective byte enables are asserted as per write strobes                   
	                -- slave registor 2
	                slv_reg2(byte_index*8+7 downto byte_index*8) <= S_AXI_WDATA(byte_index*8+7 downto byte_index*8);
	              end if;
	            end loop;
	          when b"11" =>
	            for byte_index in 0 to (C_S_AXI_DATA_WIDTH/8-1) loop
	              if ( S_AXI_WSTRB(byte_index) = '1' ) then
	                -- Respective byte enables are asserted as per write strobes                   
	                -- slave registor 3
	                slv_reg3(byte_index*8+7 downto byte_index*8) <= S_AXI_WDATA(byte_index*8+7 downto byte_index*8);
	              end if;
	            end loop;
	          when others =>
	            slv_reg0 <= slv_reg0;
	            slv_reg1 <= slv_reg1;
	            slv_reg2 <= slv_reg2;
	            slv_reg3 <= slv_reg3;
	        end case;
        else
            slv_reg0(8)<='0';
	      end if;
	    end if;
	  end if;                   
	end process; 

	-- Implement write response logic generation
	-- The write response and response valid signals are asserted by the slave 
	-- when axi_wready, S_AXI_WVALID, axi_wready and S_AXI_WVALID are asserted.  
	-- This marks the acceptance of address and indicates the status of 
	-- write transaction.

	process (S_AXI_ACLK)
	begin
	  if rising_edge(S_AXI_ACLK) then 
	    if S_AXI_ARESETN = '0' then
	      axi_bvalid  <= '0';
	      axi_bresp   <= "00"; --need to work more on the responses
	    else
	      if (axi_awready = '1' and S_AXI_AWVALID = '1' and axi_wready = '1' and S_AXI_WVALID = '1' and axi_bvalid = '0'  ) then
	        axi_bvalid <= '1';
	        axi_bresp  <= "00"; 
	      elsif (S_AXI_BREADY = '1' and axi_bvalid = '1') then   --check if bready is asserted while bvalid is high)
	        axi_bvalid <= '0';                                 -- (there is a possibility that bready is always asserted high)
	      end if;
	    end if;
	  end if;                   
	end process; 

	-- Implement axi_arready generation
	-- axi_arready is asserted for one S_AXI_ACLK clock cycle when
	-- S_AXI_ARVALID is asserted. axi_awready is 
	-- de-asserted when reset (active low) is asserted. 
	-- The read address is also latched when S_AXI_ARVALID is 
	-- asserted. axi_araddr is reset to zero on reset assertion.

	process (S_AXI_ACLK)
	begin
	  if rising_edge(S_AXI_ACLK) then 
	    if S_AXI_ARESETN = '0' then
	      axi_arready <= '0';
	      axi_araddr  <= (others => '1');
	    else
	      if (axi_arready = '0' and S_AXI_ARVALID = '1') then
	        -- indicates that the slave has acceped the valid read address
	        axi_arready <= '1';
	        -- Read Address latching 
	        axi_araddr  <= S_AXI_ARADDR;           
	      else
	        axi_arready <= '0';
	      end if;
	    end if;
	  end if;                   
	end process; 

	-- Implement axi_arvalid generation
	-- axi_rvalid is asserted for one S_AXI_ACLK clock cycle when both 
	-- S_AXI_ARVALID and axi_arready are asserted. The slave registers 
	-- data are available on the axi_rdata bus at this instance. The 
	-- assertion of axi_rvalid marks the validity of read data on the 
	-- bus and axi_rresp indicates the status of read transaction.axi_rvalid 
	-- is deasserted on reset (active low). axi_rresp and axi_rdata are 
	-- cleared to zero on reset (active low).  
	process (S_AXI_ACLK)
	begin
	  if rising_edge(S_AXI_ACLK) then
	    if S_AXI_ARESETN = '0' then
	      axi_rvalid <= '0';
	      axi_rresp  <= "00";
	    else
	      if (axi_arready = '1' and S_AXI_ARVALID = '1' and axi_rvalid = '0') then
	        -- Valid read data is available at the read data bus
	        axi_rvalid <= '1';
	        axi_rresp  <= "00"; -- 'OKAY' response
	      elsif (axi_rvalid = '1' and S_AXI_RREADY = '1') then
	        -- Read data is accepted by the master
	        axi_rvalid <= '0';
	      end if;            
	    end if;
	  end if;
	end process;

	-- Implement memory mapped register select and read logic generation
	-- Slave register read enable is asserted when valid address is available
	-- and the slave is ready to accept the read address.
	slv_reg_rden <= axi_arready and S_AXI_ARVALID and (not axi_rvalid) ;

	process (slv_reg0, slv_reg1, slv_reg2, slv_reg3, axi_araddr, S_AXI_ARESETN, slv_reg_rden)
	variable loc_addr :std_logic_vector(OPT_MEM_ADDR_BITS downto 0);
	begin
	    -- Address decoding for reading registers
	    loc_addr := axi_araddr(ADDR_LSB + OPT_MEM_ADDR_BITS downto ADDR_LSB);
	    case loc_addr is
	      when b"00" =>
	        reg_data_out <= slv_reg0;
	      when b"01" =>
	        reg_data_out <= B_ip;
	      when b"10" =>
	        reg_data_out <= slv_reg2;
	      when b"11" =>
	        reg_data_out <= slv_reg3;
	      when others =>
	        reg_data_out  <= (others => '0');
	    end case;
	end process; 

	-- Output register or memory read data
	process( S_AXI_ACLK ) is
	begin
	  if (rising_edge (S_AXI_ACLK)) then
	    if ( S_AXI_ARESETN = '0' ) then
	      axi_rdata  <= (others => '0');
	    else
	        if (slv_reg_rden = '1') then   -- slv_reg_rden <= axi_arready and S_AXI_ARVALID and (not axi_rvalid) ;
	        -- When there is a valid read address (S_AXI_ARVALID) with 
	        -- acceptance of read address by the slave (axi_arready), 
	        -- output the read dada 
	        -- Read address mux
	          axi_rdata <= reg_data_out;     -- register read data
	          
            else
                slv_reg1<=slv_reg1; --EDITED
	       end if;   
	    end if;
	  end if;
	end process;


	-- Add user logic here

    FIR1: FIR port map (
        clk=>S_AXI_ACLK,
        rst=>rst_ip,
        valid_in=>valid_in_ip,
        x=>x_ip,
        ram_out=>ram_out_ip,
        rom_out=>rom_out_ip,
        y=>y_ip,
        valid_out=>valid_out_ip,
        we=>we_ip,
        mac_init=>mac_init_ip,
        ram_address=>ram_address_ip
    );

    process(S_AXI_ACLK) is
    begin
        A_ip<=slv_reg0;
        x_ip<=A_ip(7 downto 0);
        valid_in_ip<=A_ip(8);
        rst_ip<=A_ip(9);
        
        B_ip(16 downto 0)<=y_ip;
        B_ip(17)<=valid_out_ip;
        B_ip(31 downto 18)<="00000000000000";
    end process;

	-- User logic ends

end arch_imp;
