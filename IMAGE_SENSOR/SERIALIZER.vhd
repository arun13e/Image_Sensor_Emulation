----------------------------------------------------------------------------
--  SERIALIZER.vhd
--
--  This program is free software: you can redistribute it and/or
--  modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation, either version
--  2 of the License, or (at your option) any later version.
--
----------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE ieee.numeric_std.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY SERIALIZER IS
    PORT (
        clk, rst, start, nxt_px : IN std_logic;
        mode_sr : IN std_logic_vector(6 DOWNTO 0); --mode for operation 64,32,16,8,4,2,1 and dual or one sided
        lvds_out : OUT std_logic_vector(63 DOWNTO 0);
        shift : OUT std_logic_vector(127 DOWNTO 0);
        dval, lval : OUT std_logic;
        pxdata : IN std_logic_vector(127 DOWNTO 0)
    );
END SERIALIZER;

ARCHITECTURE Behavioral OF SERIALIZER IS
    SIGNAL px_out, sh_out : std_logic_vector(63 DOWNTO 0);
    SIGNAL px_burst : std_logic;
    TYPE memory IS ARRAY (0 TO 15) OF std_logic_vector(63 DOWNTO 0); --mask to select channel for lvds output
    CONSTANT filter : memory := (
    0 => x"FFFFFFFFFFFFFFFF",
    1 => "0101010101010101010101010101010101010101010101010101010101010101",
    2 => x"1111111111111111",
    3 => x"0101010101010101",
    4 => x"0001000100010001",
    5 => x"0000000100000001",
    6 => x"0000000000000001",
    OTHERS => x"FFFFFFFFFFFFFFFF");
BEGIN

    px_mux : PROCESS (clk, pxdata, rst, nxt_px, mode_sr) IS
        VARIABLE px_count : INTEGER RANGE 0 TO 127 := 0;
    BEGIN

        IF rst = '0' THEN
            px_count := 0;

        ELSIF falling_edge(clk) THEN

            IF nxt_px = '1' THEN
                px_count := px_count + 1; --increment on every completed pixel
            END IF;

            CASE mode_sr(6 DOWNTO 4) IS
                WHEN "000" =>

                    FOR I IN 0 TO 63 LOOP

                        IF px_count < 64 THEN
                            px_out(I) <= pxdata(I * 2);
                            shift(I * 2) <= sh_out(I);
                        ELSE
                            px_out(I) <= pxdata((I * 2) + 1);
                            shift((I * 2) + 1) <= sh_out(I);
                        END IF;

                    END LOOP;

                WHEN OTHERS =>
            END CASE;

            IF px_count = 127 THEN
                px_burst <= '1'; --end of one complete burst of 128 pixels
                px_count := 0;
            ELSE
                px_burst <= '0';
            END IF;

        END IF;
    END PROCESS;
    burst_gen : PROCESS (clk, px_out, rst, px_burst, start) IS
        VARIABLE last_shift : std_logic := '0';
        VARIABLE strt_flag : std_logic := '0';
        VARIABLE init_flag : std_logic := '1';
        VARIABLE mask : std_logic_vector(63 DOWNTO 0);
        VARIABLE temp : INTEGER RANGE 0 TO 63 := 0;
    BEGIN

        IF rst = '0' THEN
            strt_flag := '0';
            init_flag := '1';
            dval <= '0';
        ELSIF rising_edge(clk) THEN

            IF start = '1' THEN
                strt_flag := '1';
            END IF;

            IF strt_flag = '1' THEN

                IF init_flag = '1' THEN
                    init_flag := '0';
                    mask := filter(to_integer(unsigned(mode_sr(3 DOWNTO 1)))); --fetching appropriate mask from rom
                    sh_out <= filter(to_integer(unsigned(mode_sr(3 DOWNTO 1))));
                    dval <= '1';
                END IF;

                IF px_burst = '1' THEN

                    IF mask(63) = '1' THEN
                        sh_out <= x"0000000000000000";
                        strt_flag := '0';
                        init_flag := '1';
                        dval <= '0';
                    ELSE --Insert overhead time and set dval = 0
                        mask := std_logic_vector(unsigned(mask) SLL 1); --shift until last channel=1
                        sh_out <= mask;
                        dval <= '1';
                    END IF;
                END IF;

                FOR I IN 0 TO 63 LOOP

                    IF to_integer(unsigned(mode_sr(3 DOWNTO 1))) = 0 THEN
                        lvds_out(I) <= px_out(I);
                    ELSE

                        IF sh_out(I) = '1' THEN --only those channels with valid data are muxed
                            temp := (I/to_integer(unsigned(mode_sr(3 DOWNTO 1)))) * to_integer(unsigned(mode_sr(3 DOWNTO 1)));
                            lvds_out(temp) <= px_out(I);
                        END IF;

                    END IF;
                END LOOP;

            END IF;
            lval <= strt_flag;
        END IF;
    END PROCESS;
END Behavioral;
