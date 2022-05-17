library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-------------------------------------------------------------------------------

entity fir_filter_4 is
    port (
        i_clk   : in  std_logic; -- system clock
        i_rstb  : in  std_logic; -- asynchronous active low reset
	
    	i_valid : in  std_logic; -- in data valid
    	o_valid : out std_logic; -- out data valid

        i_coeff_0 : in  std_logic_vector(7 downto 0);
        i_coeff_1 : in  std_logic_vector(7 downto 0);
        i_coeff_2 : in  std_logic_vector(7 downto 0);
        i_coeff_3 : in  std_logic_vector(7 downto 0);
 
        i_data  : in  std_logic_vector(7 downto 0); -- input data
        o_data  : out std_logic_vector(7 downto 0)  -- output data
    );
end fir_filter_4;

-------------------------------------------------------------------------------

architecture rtl of fir_filter_4 is
	-- type: data pipeline and coefficients
    type pip_data	is array (0 to 3) of signed(7 downto 0);
    type coeffs		is array (0 to 3) of signed(7 downto 0);
	-- type: sum and multiplication arrays
    type mult 	is array (0 to 3) of signed(2*8-1 downto 0);
    type sum	is array (0 to 1) of signed(2*8 downto 0); 
    	-- signal declaration
    signal coeff	: coeffs;
    signal data		: pip_data; -- pipeline of historic data values
    signal conv		: mult;	    -- array of (coefficient*data) products
    signal sum0		: sum;
    signal sum1		: signed(2*8+1 downto 0); 

begin

    data_input : process (i_rstb, i_clk)
    begin

        if(i_rstb = '0') then -- reset all signals
            data  <= (others => (others => '0')); -- clear data pipeline values
            coeff <= (others => (others => '0')); -- clear coefficient registers
        elsif(rising_edge(i_clk)) then -- insert new sample at the beginning, shift the others
      	    if i_valid = '1' then
    		data     <= signed(i_data)&data(0 to data'length-2); -- shift new data into data pipeline
		coeff(0) <= signed(i_coeff_0); --input coefficients
		coeff(1) <= signed(i_coeff_1);
		coeff(2) <= signed(i_coeff_2);
		coeff(3) <= signed(i_coeff_3);
            end if;
  	end if;
    end process data_input;

    convolution : process (i_rstb, i_clk)
    begin

        if(i_rstb = '0') then
            conv <= (others => (others => '0'));
        elsif(rising_edge(i_clk)) then
	    if i_valid = '1' then
                for k in 0 to 3 loop
                    conv(k) <= data(k) * coeff(k); -- perform convolution
            	end loop;
	    end if;
        end if;
    end process convolution;

    add0 : process (i_rstb, i_clk)
    begin

        if(i_rstb = '0') then
            sum0 <= (others => (others => '0'));
        elsif(rising_edge(i_clk)) then
	    if i_valid = '1' then
                for k in 0 to 1 loop
                    sum0(k) <= resize(conv(2*k), 2*8+1) + resize(conv(2*k+1), 2*8+1);
            	end loop;
	    end if;
        end if;
    end process add0;

    add1 : process (i_rstb, i_clk)
    begin

        if(i_rstb = '0') then
            sum1 <= (others => '0');
        elsif(rising_edge(i_clk)) then
	    if i_valid = '1' then
	        sum1 <= resize(sum0(0), 2*8+2) + resize(sum0(1), 2*8+2);
	    end if;
        end if;
    end process add1;

    data_output : process (i_rstb, i_clk)
    begin

        if(i_rstb = '0') then
            o_data <= (others => '0');
        elsif(rising_edge(i_clk)) then
	    if i_valid = '1' then
                o_valid <= '1';
		o_data <= std_logic_vector(sum1(17 downto 10));
	    else o_valid <= '0';
            end if;
	end if;
    end process data_output;
    
end rtl;
