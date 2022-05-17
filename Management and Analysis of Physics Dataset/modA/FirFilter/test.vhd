library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use STD.textio.all;
-------------------------------------------------------------------------------

entity test is

end entity test;

-------------------------------------------------------------------------------

architecture test of test is

    signal CLK100MHZ    : std_logic := '0';
    signal uart_txd_in  : std_logic;
    signal uart_rxd_out : std_logic;
    file file_VECTORS   : text;
    file file_RESULTS   : text;

begin  -- architecture test


  -- component instantiation
  DUT : entity work.top
    port map (
      CLK100MHZ      => CLK100MHZ,       
      uart_txd_in    => uart_txd_in,      
      uart_rxd_out   => uart_rxd_out);       

  -- clock generation
  CLK100MHZ <= not CLK100MHZ after 10 ns;


  -- waveform generation
  WaveGen_Proc : process
    variable v_ILINE        : line;
    variable v_OLINE        : line;
    variable i_data_integer : integer   := 0;
    variable o_data_integer : integer   := 0;
    variable i_data_slv     : std_logic_vector(7 downto 0) := (others => '0');
    variable o_data_slv     : std_logic_vector(7 downto 0) := X"09";
    variable count          : integer := 0;
    constant c_WIDTH        : natural   := 8;
    constant divisor        : natural   := 867;
    constant divisor2       : natural   := 1735;
  

  begin
    -- insert signal assignments here
    file_open(file_VECTORS, "input_vectors.txt", read_mode);
    file_open(file_RESULTS, "output_results.txt", write_mode);
    wait until rising_edge(CLK100MHZ);
    data : while not endfile(file_VECTORS) loop
      readline(file_VECTORS, v_ILINE);
      read(v_ILINE, i_data_integer);
      i_data_slv         := std_logic_vector(to_signed(i_data_integer, i_data_slv'length));
	wait until rising_edge(CLK100MHZ);
	uart_txd_in <= '0';
	while (count < divisor) loop
    			wait until rising_edge(CLK100MHZ);
			count := count + 1; 	
		end loop;
	count := 0;	
	i_o : for i in 0 to 7 loop
		while (count < divisor) loop
    			wait until rising_edge(CLK100MHZ);
			uart_txd_in     <= std_logic(i_data_slv(i));
			count := count + 1; 	
		end loop;
	count := 0;
	end loop i_o;

	uart_txd_in <= '1';
	wait until rising_edge(CLK100MHZ);

	wait until uart_rxd_out = '0';
	wait until rising_edge(CLK100MHZ);
	wait until rising_edge(CLK100MHZ);

	o_i : for k in 0 to 7 loop
		while (count < divisor) loop
    			wait until rising_edge(CLK100MHZ);
			o_data_slv(k)     := std_logic(uart_rxd_out);
			count := count + 1; 	
		end loop;
	count := 0;
	end loop o_i;
      o_data_integer := to_integer(signed(o_data_slv));
      write(v_OLINE, o_data_integer, left, c_WIDTH);
      writeline(file_RESULTS, v_OLINE);
    end loop data;
    file_close(file_VECTORS);
    file_close(file_RESULTS);
    wait;
  end process WaveGen_Proc;

end architecture test;
