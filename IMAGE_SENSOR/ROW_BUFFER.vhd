----------------------------------------------------------------------------
--  ROW_BUFFER.vhd
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
USE IEEE.std_logic_unsigned.ALL;

ENTITY ROW_BUFFER IS
    PORT (
        clk, wr_buf, load : IN std_logic;
        rd_ram, ready, nxt_px, load_run : OUT std_logic; --ready indicator for Sequencer
        bit_md : IN std_logic_vector(1 DOWNTO 0); --bit_md: 8/10/12 bit per pixel
        data : IN std_logic_vector(11 DOWNTO 0);
        shift : IN std_logic_vector(127 DOWNTO 0); --shift control lines
        rm_addr : OUT std_logic_vector(12 DOWNTO 0); --address for ram
        pxdata : OUT std_logic_vector(127 DOWNTO 0)); --pixel data for serializer
END ROW_BUFFER;

ARCHITECTURE Behavioral OF ROW_BUFFER IS
    SUBTYPE row IS std_logic_vector(11 DOWNTO 0);
    TYPE mem IS ARRAY (INTEGER RANGE 0 TO 127, INTEGER RANGE 0 TO 63) OF row;
    SIGNAL row_var : mem;
    TYPE sh_reg IS ARRAY (INTEGER RANGE 0 TO 127) OF std_logic_vector(11 DOWNTO 0);

    --	SIGNAL ram_wr, ram_clk : std_logic;
    --	SIGNAL ram_addr: std_logic_vector(12 downto 0) ;
    --	SIGNAL ram_din : std_logic_vector(11 downto 0) ;
    --	SIGNAL ram_dout: std_logic_vector(11 downto 0) ;

BEGIN
    --    cmv_sensor : entity work.block_ram_wrapper
    --	PORT MAP(
    --	    clka  => ram_clk,
    --        wea   => ram_wr,
    --        addra => ram_addr,
    --        dina  => ram_din,
    --        douta => ram_dout );

    main_proc : PROCESS (clk, data, wr_buf, load, bit_md, shift) IS
        VARIABLE shift_flag : std_logic := '0';
        VARIABLE shift_count : INTEGER RANGE 0 TO 11 := 0;
        VARIABLE shift_reg : sh_reg;
        VARIABLE ld_flag, wr_flag, last_wr_flag : std_logic := '0';
        VARIABLE bit_ch : INTEGER RANGE 0 TO 64 := 0;
        VARIABLE addr : std_logic_vector(12 DOWNTO 0) := (OTHERS => '0');
    BEGIN
        IF rising_edge(clk) THEN
            load_run <= ld_flag;

            IF load = '1' THEN --sets ld_flag, also serves as reset
                ready <= '0';
                ld_flag := '1';
                addr := (OTHERS => '0');
                nxt_px <= '0';
                bit_ch := 0;
                shift_count := 0;
                last_wr_flag := '0';
            END IF;

            IF ld_flag = '1' THEN --stays in this control until buffer filled

                IF wr_flag = '1' THEN --buffer write cycle
                    rd_ram <= '0';

                    IF wr_buf = '1' THEN

                        row_var((to_integer(unsigned(addr(12 DOWNTO 6)))), (to_integer(unsigned(addr(5 DOWNTO 0))))) <= data;
                        wr_flag := '0';
                        addr := addr + '1'; --next ram address to access
                    END IF;

                ELSIF last_wr_flag = '1' THEN

                    FOR I IN 0 TO 127 LOOP --updating shift register after last write
                        shift_reg(I) := row_var(I, bit_ch);
                    END LOOP;

                    ready <= '1';
                    ld_flag := '0';
                ELSE
                    rd_ram <= '1';
                    rm_addr <= addr;
                    wr_flag := '1';

                    IF addr = x"1FFF" THEN
                        last_wr_flag := '1';
                    END IF;
                END IF;
            ELSE
                ready <= '0';--
                FOR I IN 0 TO 127 LOOP

                    IF shift(I) = '1' THEN
                        shift_reg(I) := x"555";
                        --shift_reg(I) := std_logic_vector(unsigned(shift_reg(I)) SRL 1);
                        shift_flag := shift_flag OR shift(I); --1 if even a single shift operation occurs
                    END IF;

                    pxdata(I) <= shift_reg(I)(0); --assigning least significant bit to output
                END LOOP;

                IF shift_flag = '1' THEN
                    shift_count := shift_count + 1; --increment counter if flag 1
                    shift_flag := '0';
                END IF;

                IF shift_count = 12 - (2 * to_integer(unsigned(bit_md))) THEN
                    nxt_px <= '1';
                    bit_ch := bit_ch + 1;
                    shift_count := 0;

                    IF bit_ch = 64 THEN
                        bit_ch := 0;
                    END IF;

                    FOR I IN 0 TO 127 LOOP
                        shift_reg(I) := x"555";
                        --shift_reg(I) := row_var(I, bit_ch);
                    END LOOP;

                ELSE
                    nxt_px <= '0';
                END IF;

            END IF;
        END IF;
    END PROCESS;
END Behavioral;
