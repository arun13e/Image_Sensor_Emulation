----------------------------------------------------------------------------
--  random_data.vhd
--
--  This program is free software: you can redistribute it and/or
--  modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation, either version
--  2 of the License, or (at your option) any later version.
--
----------------------------------------------------------------------------

LIBRARY IEEE;
USE work.lfsr_pkg.ALL;
USE IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

ENTITY random_data IS
	PORT (
		clk, rd : IN std_logic;
		wr : OUT std_logic;
		addr : IN std_logic_vector(12 DOWNTO 0);
		r_data : OUT std_logic_vector(11 DOWNTO 0));

END random_data;

ARCHITECTURE Behavioral OF random_data IS

BEGIN

	main_proc : PROCESS (clk, rd) IS
		VARIABLE reg : std_logic_vector(11 DOWNTO 0) := x"5CA";
	BEGIN
		IF rising_edge(clk) THEN
			IF rd = '1' THEN
				wr <= '1';
				reg := rand_shift(reg);
				r_data <= reg;
			ELSE
				wr <= '0';
			END IF;
		END IF;
	END PROCESS;

END Behavioral;
