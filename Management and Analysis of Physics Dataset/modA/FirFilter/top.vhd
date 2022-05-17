library ieee;
use ieee.std_logic_1164.all;



entity top is

  port (

    CLK100MHZ    : in  std_logic;
    uart_txd_in  : in  std_logic;
    uart_rxd_out : out std_logic);

end entity top;

architecture str of top is
  signal clock        : std_logic;
  signal rough_data   : std_logic_vector(7 downto 0) := X"61";
  signal data_valid_rf: std_logic;
  signal data_valid_ft: std_logic;
  signal busy         : std_logic;
  signal uart_tx      : std_logic;
  signal i_coeff_0    : std_logic_vector(7 downto 0) := X"05";   -- 05
  signal i_coeff_1    : std_logic_vector(7 downto 0) := X"2D";   -- 45
  signal i_coeff_2    : std_logic_vector(7 downto 0) := X"2D";   -- 45
  signal i_coeff_3    : std_logic_vector(7 downto 0) := X"05";   -- 05
  signal i_rstb       : std_logic := '1';
  
  signal filtered_data: std_logic_vector(7 downto 0);

  
  

  component uart_transmitter is
    port (
      clock        : in  std_logic;
      data_to_send : in  std_logic_vector(7 downto 0);
      data_valid   : in  std_logic;
      busy         : out std_logic;
      uart_tx      : out std_logic);
  end component uart_transmitter;

  component fir_filter_4 is
    port (
      i_clk        : in  std_logic;
      i_rstb	   : in  std_logic;
      i_coeff_0	   : in  std_logic_vector(7 downto 0);
      i_coeff_1	   : in  std_logic_vector(7 downto 0);
      i_coeff_2	   : in  std_logic_vector(7 downto 0);
      i_coeff_3	   : in  std_logic_vector(7 downto 0);
      i_data       : in  std_logic_vector(7 downto 0);
      i_valid      : in  std_logic;
      o_data       : out std_logic_vector(7 downto 0);
      o_valid      : out std_logic);
  end component fir_filter_4;

  component uart_receiver is
    port (
      clock         : in  std_logic;
      uart_rx       : in  std_logic;
      valid         : out std_logic;
      received_data : out std_logic_vector(7 downto 0));
  end component uart_receiver;

begin  -- architecture str

  uart_receiver_1 : uart_receiver
    port map (
      clock         => CLK100MHZ,
      uart_rx       => uart_txd_in,
      valid         => data_valid_rf,
      received_data => rough_data);

  fir_filter_4_1 : fir_filter_4
  port map (
      i_clk         => CLK100MHZ,
      i_rstb        => i_rstb, 
      i_coeff_0     => i_coeff_0,
      i_coeff_1     => i_coeff_1,
      i_coeff_2     => i_coeff_2,
      i_coeff_3     => i_coeff_3,
      i_valid       => data_valid_rf,
      o_valid       => data_valid_ft,
      i_data        => rough_data,     
      o_data        => filtered_data);   


  uart_transmitter_1 : uart_transmitter
    port map (
      clock        => CLK100MHZ,
      data_to_send => filtered_data,
      data_valid   => data_valid_ft,
      busy         => busy,
      uart_tx      => uart_rxd_out);

end architecture str;
