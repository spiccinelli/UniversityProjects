-------------------------------------------------------------------------------
-- Title      : Testbench for design "uart_transmitter"
-- Project    : 
-------------------------------------------------------------------------------
-- File       : uart_transmitter_tb.vhd
-- Author     : Antonio  <antonio@dhcp-35.pd.infn.it>
-- Company    : 
-- Created    : 2020-12-14
-- Last update: 2020-12-14
-- Platform   : 
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2020 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2020-12-14  1.0      antonio	Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

-------------------------------------------------------------------------------

entity uart_transmitter_tb is

end entity uart_transmitter_tb;

-------------------------------------------------------------------------------

architecture test of uart_transmitter_tb is

  component uart_transmitter is
    port (
      clock        : in  std_logic;
      data_to_send : in  std_logic_vector(7 downto 0);
      data_valid   : in  std_logic;
      busy         : out std_logic;
      uart_tx      : out std_logic);
  end component uart_transmitter;
  
  -- component ports
  signal clock        : std_logic :='0';
  signal data_to_send : std_logic_vector(7 downto 0) :=X"61";
  signal data_valid   : std_logic:='0';
  signal busy         : std_logic;
  signal uart_tx      : std_logic;


begin  -- architecture test
  -- component instantiation
  DUT: uart_transmitter
    port map (
      clock        => clock,
      data_to_send => data_to_send,
      data_valid   => data_valid,
      busy         => busy,
      uart_tx      => uart_tx);

  -- clock generation
  clock <= not clock after 5 ns;

  main: process  is
  begin  -- process main

    wait for 1 us;
    wait until rising_edge(clock);
    data_valid <='1';
    wait until rising_edge(clock);
    data_valid <='0';
    wait until busy ='1';
    wait until busy ='0';
    wait for 1 us;
    wait;
  end process main;

end architecture test;

-------------------------------------------------------------------------------

configuration uart_transmitter_tb_test_cfg of uart_transmitter_tb is
  for test
  end for;
end uart_transmitter_tb_test_cfg;

-------------------------------------------------------------------------------
