----------------------------------------------------------------------------
--  LFSR.vhd
--
--  This program is free software: you can redistribute it and/or
--  modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation, either version
--  2 of the License, or (at your option) any later version.
--
----------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;

PACKAGE lfsr_pkg IS

	FUNCTION rand_shift (DATA : std_logic_vector) RETURN std_logic_vector;

END;

PACKAGE BODY lfsr_pkg IS

	FUNCTION rand_shift (DATA : std_logic_vector) RETURN std_logic_vector IS
		VARIABLE feedback : std_logic := '1' ;
		VARIABLE out_reg : std_logic_vector(11 DOWNTO 0) := DATA;
	BEGIN

		--feedback := NOT (out_reg(11) XOR out_reg(10));
		out_reg := out_reg(10 DOWNTO 0) & feedback;

		RETURN out_reg;

	END FUNCTION;
END PACKAGE BODY;
