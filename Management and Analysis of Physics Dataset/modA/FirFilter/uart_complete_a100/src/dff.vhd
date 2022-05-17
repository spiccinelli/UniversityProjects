library ieee;
use ieee.std_logic_1164.all;
entity dff is
	port (
	clk : in std_logic;
	rst : in std_logic;
	d : in std_logic;
	q : out std_logic);
end entity dff;
architecture rtl of dff is
begin
	flipflop : process (clk) is
	begin
		if rising_edge(clk) then
			if rst = '0' then
				q <= '0';
			else
				q <= d;
			end if;
		end if;
	end process flipflop;
end architecture rtl;
