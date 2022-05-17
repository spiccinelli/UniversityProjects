library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use STD.textio.all;
library work;
-------------------------------------------------------------------------------

entity top_tb is

end entity top_tb;

-------------------------------------------------------------------------------

architecture test of top_tb is

  component top is
      port (
          CLK100MHZ    : in  std_logic;
          uart_txd_in  : in  std_logic;
          uart_rxd_out : out std_logic
      	  );
  end component top;


  -- component ports
  
  signal uart_rx    : std_logic;
  signal uart_tx    : std_logic;
  signal i_clk      : std_logic := '0';
  signal clk_enable : boolean := true;
  signal data_to_r  : std_logic_vector(7 downto 0) := (others => '0');
  signal data_from_t : std_logic_vector(7 downto 0) := (others => '0');
  constant c_WIDTH  : natural := 8;
  file file_VECTORS : text;
  file file_RESULTS : text;

  -- clock

begin  -- architecture test

  -- component instantiation
  DUT : top
    port map (
      CLK100MHZ     => i_clk,
      uart_txd_in   => uart_rx,
      uart_rxd_out  => uart_tx
      );         

  -- clock generation
  i_clk <= not i_clk after 10 ns when clk_enable = true
           else '0';
  -- waveform generation
  WaveGen_Proc : process
    variable CurrentLine    : line;
    variable v_ILINE        : line;
    variable v_OLINE        : line;
    variable i_data_integer : integer := 0;
    variable o_data_integer : integer := 0;
    variable i_data_slv     : std_logic_vector(7 downto 0) := (others => '0');
  begin
    -- insert signal assignments here
    file_open(file_VECTORS, "input_vectors.txt", read_mode);
    file_open(file_RESULTS, "output_results.txt", write_mode);
    wait until rising_edge(i_clk);
    --wait until rising_edge(i_clk);

    while not endfile(file_VECTORS) loop
      readline(file_VECTORS, v_ILINE);
      read(v_ILINE, i_data_integer);
      data_to_r <= std_logic_vector(to_signed(i_data_integer, data_to_r'length));
      wait until rising_edge(i_clk);
      uart_rx <= '0';
      wait for 8680 ns;
      for k in 0 to 7 loop
	      uart_rx <= data_to_r(k);
	      wait for 8680 ns;
      end loop;
      uart_rx <= '1';

      wait until rising_edge(i_clk);

      wait until uart_tx = '0';
      wait until rising_edge(i_clk);
      wait until rising_edge(i_clk);

      for k in 0 to 7 loop
	      data_from_t(k) <= uart_tx;
	      wait for 8680 ns;
      end loop;
      
      o_data_integer := to_integer(signed(data_from_t));
      write(v_OLINE, o_data_integer, left, c_WIDTH);
      writeline(file_RESULTS, v_OLINE);
    end loop;
    file_close(file_VECTORS);
    file_close(file_RESULTS);
    clk_enable <= false;
    wait;
  end process WaveGen_Proc;

end architecture test;
