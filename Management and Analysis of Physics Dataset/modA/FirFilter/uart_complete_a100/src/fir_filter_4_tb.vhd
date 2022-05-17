library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use STD.textio.all;
library work;
-------------------------------------------------------------------------------

entity fir_filter_4_tb is

end entity fir_filter_4_tb;

-------------------------------------------------------------------------------

architecture test of fir_filter_4_tb is

  component fir_filter_4 is
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
  end component fir_filter_4;


  -- component ports
  signal i_clk      : std_logic                    := '0';    -- [in]
  signal i_rstb     : std_logic;                              -- [in]
  signal i_coeff_0  : std_logic_vector(7 downto 0) := X"05";  -- [in]
  signal i_coeff_1  : std_logic_vector(7 downto 0) := X"2D";  -- [in]
  signal i_coeff_2  : std_logic_vector(7 downto 0) := X"2D";  -- [in]
  signal i_coeff_3  : std_logic_vector(7 downto 0) := X"05";  -- [in]
  signal i_valid    : std_logic			   := '0';
  signal o_valid    : std_logic			   := '0';
  signal i_data     : std_logic_vector(7 downto 0);	      -- [in]
  signal o_data     : std_logic_vector(7 downto 0);           -- [out]
  signal clk_enable : boolean                      := true; 
  constant c_WIDTH  : natural                      := 8;
  file file_VECTORS : text;
  file file_RESULTS : text;

  -- clock

begin  -- architecture test

  -- component instantiation
  DUT : fir_filter_4
    port map (
      i_clk     => i_clk,       
      i_rstb    => i_rstb,      
      i_coeff_0 => i_coeff_0,       
      i_coeff_1 => i_coeff_1,       
      i_coeff_2 => i_coeff_2,       
      i_coeff_3 => i_coeff_3,
      i_valid   => i_valid,
      o_valid   => o_valid,    
      i_data    => i_data,          
      o_data    => o_data);         

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
    variable i_data_slv     : std_logic_vector(7 downto 0);
  begin
    -- insert signal assignments here
    file_open(file_VECTORS, "input_vectors.txt", read_mode);
    file_open(file_RESULTS, "output_results.txt", write_mode);
    i_rstb <= '0';
    wait until rising_edge(i_clk);
    wait until rising_edge(i_clk);
    i_rstb  <= '1';
    i_valid <= '1';
    while not endfile(file_VECTORS) loop
      readline(file_VECTORS, v_ILINE);
      read(v_ILINE, i_data_integer);
      i_data         <= std_logic_vector(to_signed(i_data_integer, i_data'length));
      wait until rising_edge(i_clk);
      o_data_integer := to_integer(signed(o_data));
      write(v_OLINE, o_data_integer, left, c_WIDTH);
      writeline(file_RESULTS, v_OLINE);
    end loop;
    i_valid <= '0';
    file_close(file_VECTORS);
    file_close(file_RESULTS);
    clk_enable <= false;
    wait;
  end process WaveGen_Proc;



end architecture test;
