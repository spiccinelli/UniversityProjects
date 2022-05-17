library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity fir_filter_4 is

  port (
    i_clk        : in  std_logic;
    i_rstb	 : in  std_logic;
    i_valid      : in  std_logic;

    i_coeff_0	 : in  std_logic_vector(7 downto 0);
    i_coeff_1	 : in  std_logic_vector(7 downto 0);
    i_coeff_2	 : in  std_logic_vector(7 downto 0);
    i_coeff_3	 : in  std_logic_vector(7 downto 0);

    i_data       : in  std_logic_vector(7 downto 0);
    o_data       : out std_logic_vector(7 downto 0);
    o_valid      : out std_logic);

end entity fir_filter_4;

architecture firter of fir_filter_4 is

    type t_4_vec is array(0 to 3) of signed(7 downto 0);

    signal coeff        : t_4_vec := (others => (others => '0'));
    signal s_data       : t_4_vec := (others => (others => '0'));
    signal result       : std_logic_vector(17 downto 0);


begin

  in_p : process (i_rstb, i_clk) is
    begin

	if    (i_rstb = '0') then
	      s_data      <= (others => (others => '0'));
	      coeff     <= (others => (others => '0'));

	elsif (rising_edge(i_clk)) then
	   if i_valid = '1' then                                         --!

	      s_data(0)   <= signed(i_data);
	      s_data(1)   <= s_data(0);
	      s_data(2)   <= s_data(1);
	      s_data(3)   <= s_data(2);
	      
              coeff(0)    <= signed(i_coeff_0);
	      coeff(1)    <= signed(i_coeff_1);
	      coeff(2)    <= signed(i_coeff_2);
	      coeff(3)    <= signed(i_coeff_3);


	   end if;
	end if;
    end process in_p;




  calculate : process (i_rstb, i_clk) is

	type t_mul_variable is array (0 to 3) of signed(15 downto 0);

	variable v_mul  : t_mul_variable;		
	variable v_sum1 : signed(16 downto 0);	
	variable v_sum2 : signed(16 downto 0);	

    begin

	if    (i_rstb = '0') then

		for k in 0 to 3 loop
			v_mul(k) := (others => '0');
		end loop;

			v_sum1  := (others => '0');
			v_sum2  := (others => '0');
 			result  <= (others => '0');
 			

	elsif (rising_edge(i_clk)) then
		if i_valid = '1' then
		for k in 0 to 3 loop
			v_mul(k) := coeff(k)*s_data(k);
		end loop;
		
		v_sum1 := resize(v_mul(0), 17) + resize(v_mul(1), 17);
		v_sum2 := resize(v_mul(2), 17) + resize(v_mul(3), 17);
		result <= std_logic_vector(resize(v_sum1,18) + resize(v_sum2,18));
		end if;
	end if;

    end process calculate;



  terminate : process (i_rstb, i_clk) is

    begin
	if    (i_rstb = '0') then
		o_data  <= ( others=>'0');
			
	elsif (rising_edge(i_clk)) then
		if i_valid = '1' then
		o_valid <= '1';
		o_data <= result(17 downto 10);
		else o_valid <= '0';
		end if;
	end if;
    end process terminate;

end architecture firter;		
	
	

















	      
	  



