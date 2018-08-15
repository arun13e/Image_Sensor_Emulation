----------------------------------------------------------------------------
--  SEQUENCER.vhd
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

ENTITY SEQUENCER IS
    GENERIC (
        rd_spi_mask : std_logic_vector(127 DOWNTO 0) := x"00000000000000000000000000000000";
        wr_spi_mask : std_logic_vector(127 DOWNTO 0) := x"00000000000000000000000000000000");

    PORT (
        ind_clk : IN std_logic;

        lvds_clk_in : IN std_logic;
        frame_req : IN std_logic;
        txp1, txp2 : OUT std_logic;
        sys_rst : IN std_logic;

        spi_en, spi_in, spi_clk : IN std_logic;
        spi_out : OUT std_logic;

        lvds_out : OUT std_logic_vector(63 DOWNTO 0);
        lvds_clk_out : OUT std_logic;
        lvds_stat : OUT std_logic);

END SEQUENCER;

ARCHITECTURE Behavioral OF SEQUENCER IS
    TYPE memory IS ARRAY (0 TO 127) OF std_logic_vector(15 DOWNTO 0);
    SIGNAL reg_arr : memory := (
    1 => x"0C00",
    67 => x"0001",
    68 => x"0009",
    71 => x"0600",
    73 => x"0600",
    79 => x"0001",
    80 => x"0001",
    82 => x"1632",
    83 => x"1705",
    84 => x"0082",
    85 => x"0082",
    86 => x"0082",
    87 => x"030C",
    88 => x"030C",
    89 => x"0055",
    90 => x"FFFF",
    91 => x"FFFF",
    92 => x"FFFF",
    93 => x"FFFF",
    94 => x"0007",
    95 => x"FFFF",
    96 => x"FFFF",
    98 => x"8888",
    99 => x"8888",
    102 => x"2040",
    103 => x"0FC0",
    104 => x"0040",
    105 => x"2040",
    106 => x"2040",
    107 => x"3060",
    108 => x"3060",
    109 => x"3060",
    110 => x"3060",
    111 => x"8888",
    113 => x"030A",
    114 => x"005F",
    116 => x"017F",
    117 => x"0004",
    118 => x"0001",
    120 => x"0009",
    121 => x"0001",
    122 => x"0020",
    124 => x"0005",
    125 => x"0002",
    126 => x"0302",
    OTHERS => x"0000");
    SIGNAL nxt_px_wire, load_run : std_logic;
    SIGNAL mode_sr_md : std_logic_vector(6 DOWNTO 0); --pixel bits/pixel orientation/burst mode/one or two sided mode
    SIGNAL shift_wire : std_logic_vector(127 DOWNTO 0);
    SIGNAL px_data_wire : std_logic_vector(127 DOWNTO 0);
    SIGNAL load, ready : std_logic;
    SIGNAL addr : std_logic_vector(6 DOWNTO 0);
    SIGNAL data : std_logic_vector(15 DOWNTO 0);
    SIGNAL spi_wr, spi_rd : std_logic;

    SIGNAL delay : std_logic_vector(23 DOWNTO 0);
    SIGNAL del_strt, intrpt : std_logic;

    SIGNAL delay1 : std_logic_vector(23 DOWNTO 0);
    SIGNAL del_strt1, intrpt1 : std_logic;

    SIGNAL delay2 : std_logic_vector(23 DOWNTO 0);
    SIGNAL del_strt2, intrpt2 : std_logic;

    SIGNAL frm_comp : std_logic;
    SIGNAL frm_count_sig : std_logic_vector(17 DOWNTO 0);

    SIGNAL status : std_logic_vector(11 DOWNTO 0);

    SIGNAL rm_addr : std_logic_vector(12 DOWNTO 0);
    SIGNAL rd_ram : std_logic;
    SIGNAL wr_buf : std_logic;
    SIGNAL r_data : std_logic_vector(11 DOWNTO 0);
    COMPONENT random_data
        PORT (
            clk, rd : IN std_logic;
            wr : OUT std_logic;
            addr : IN std_logic_vector(12 DOWNTO 0);
            r_data : OUT std_logic_vector(11 DOWNTO 0));
    END COMPONENT;

    COMPONENT SERIALIZER
        PORT (
            clk, rst, start, nxt_px : IN std_logic;
            mode_sr : IN std_logic_vector(6 DOWNTO 0);
            dval, lval : OUT std_logic;
            lvds_out : OUT std_logic_vector(63 DOWNTO 0);
            shift : OUT std_logic_vector(127 DOWNTO 0);
            pxdata : IN std_logic_vector(127 DOWNTO 0));
    END COMPONENT;

    COMPONENT ROW_BUFFER
        PORT (
            clk, wr_buf, load : IN std_logic;
            rd_ram, ready, nxt_px, load_run : OUT std_logic;
            bit_md : IN std_logic_vector(1 DOWNTO 0);
            data : IN std_logic_vector(11 DOWNTO 0);
            shift : IN std_logic_vector(127 DOWNTO 0);
            rm_addr : OUT std_logic_vector(12 DOWNTO 0);
            pxdata : OUT std_logic_vector(127 DOWNTO 0));
    END COMPONENT;

    COMPONENT SPI
        PORT (
            spi_en, spi_in, spi_clk : IN std_logic;
            addr : OUT std_logic_vector(6 DOWNTO 0);
            data : INOUT std_logic_vector(15 DOWNTO 0);
            spi_out, rd, wr : OUT std_logic);
    END COMPONENT;

BEGIN
    lvds_clk_out <= lvds_clk_in;
    status(11 DOWNTO 6) <= "000010";
    mode_sr_md <= "0000010";

    lfsr_map : random_data PORT MAP(
        clk => lvds_clk_in,
        rd => rd_ram,
        wr => wr_buf,
        addr => rm_addr,
        r_data => r_data);

    ser_map : SERIALIZER PORT MAP(
        clk => lvds_clk_in,
        rst => sys_rst,
        start => ready,
        nxt_px => nxt_px_wire,
        mode_sr => mode_sr_md(6 DOWNTO 0),
        lvds_out => lvds_out,
        shift => shift_wire,
        lval => status(1),
        dval => status(0),
        pxdata => px_data_wire);

    row_map : ROW_BUFFER PORT MAP(
        clk => lvds_clk_in,
        wr_buf => wr_buf,
        load_run => load_run,
        load => load,
        rd_ram => rd_ram,
        ready => ready,
        nxt_px => nxt_px_wire,
        bit_md => reg_arr(118)(1 DOWNTO 0),
        data => r_data,
        shift => shift_wire,
        rm_addr => rm_addr,
        pxdata => px_data_wire);

    spi_map : SPI PORT MAP(
        spi_en => spi_en,
        spi_in => spi_in,
        spi_clk => spi_clk,
        addr => addr,
        data => data,
        spi_out => spi_out,
        rd => spi_rd,
        wr => spi_wr);

    delay_proc : PROCESS (ind_clk, delay, del_strt) IS
        VARIABLE count : std_logic_vector(23 DOWNTO 0) := x"FFFFFF";
        VARIABLE init : std_logic := '1';
    BEGIN
        IF rising_edge(ind_clk) THEN
            IF del_strt = '1' AND init = '1' THEN
                count := delay;
                init := '0';
                intrpt <= '0';

            ELSIF count = x"000000" AND del_strt = '1' THEN
                intrpt <= '1';
                init := '1';

            ELSIF del_strt = '1' THEN
                count := count - 1;
                intrpt <= '0';
            ELSE
                intrpt <= '0';
                init := '1';
            END IF;
        END IF;
    END PROCESS;

    delay1_proc : PROCESS (ind_clk, delay1, del_strt1) IS
        VARIABLE count : std_logic_vector(23 DOWNTO 0) := x"FFFFFF";
        VARIABLE init : std_logic := '1';
    BEGIN
        IF rising_edge(ind_clk) THEN
            IF del_strt1 = '1' AND init = '1' THEN
                count := delay1;
                init := '0';
                intrpt1 <= '0';

            ELSIF count = x"000000" AND del_strt1 = '1' THEN
                intrpt1 <= '1';
                init := '1';

            ELSIF del_strt1 = '1' THEN
                count := count - 1;
                intrpt1 <= '0';
            ELSE
                intrpt1 <= '0';
                init := '1';
            END IF;
        END IF;
    END PROCESS;

    delay2_proc : PROCESS (ind_clk, delay2, del_strt2) IS
        VARIABLE count : std_logic_vector(23 DOWNTO 0) := x"FFFFFF";
        VARIABLE init : std_logic := '1';
    BEGIN
        IF rising_edge(ind_clk) THEN
            IF del_strt2 = '1' AND init = '1' THEN
                count := delay2;
                init := '0';
                intrpt2 <= '0';

            ELSIF count = x"000000" AND del_strt2 = '1' THEN
                intrpt2 <= '1';
                init := '1';

            ELSIF del_strt2 = '1' THEN
                count := count - 1;
                intrpt2 <= '0';
            ELSE
                intrpt2 <= '0';
                init := '1';
            END IF;
        END IF;
    END PROCESS;

    main_proc : PROCESS (lvds_clk_in, frame_req, sys_rst, ready, intrpt, spi_en) IS
        VARIABLE init1 : std_logic := '1';
        VARIABLE init2 : std_logic := '0';
        VARIABLE init3 : std_logic := '0';
        VARIABLE frm_count : INTEGER RANGE 0 TO 3 := 0;

    BEGIN

        IF falling_edge(lvds_clk_in) THEN
            IF init1 = '1' AND sys_rst = '0' THEN
                IF intrpt = '0' THEN
                    delay <= x"0000FA";
                    del_strt <= '1';
                    frm_count_sig <= "000000000000000000";
                ELSE
                    del_strt <= '0';
                    init1 := '0';
                    frm_count := 0;
                    init2 := '1';
                END IF;

            ELSIF sys_rst = '0' THEN
                frm_count := 0;
                init2 := '1';
                frm_count_sig <= "000000000000000000";

            ELSIF init2 = '1' THEN
                IF intrpt = '0' THEN
                    delay <= x"0000FA";
                    del_strt <= '1';
                ELSE
                    del_strt <= '0';
                    init2 := '0';
                END IF;

            ELSIF spi_en = '1' THEN
                init3 := '1';

            ELSIF spi_en = '0' AND init3 = '1' THEN
                IF intrpt = '0' THEN
                    delay <= x"0000FA";
                    del_strt <= '1';
                ELSE
                    del_strt <= '0';
                    init3 := '0';
                END IF;

            ELSE
                IF frame_req = '1' THEN
                    frm_count := frm_count + 1;
                END IF;

                IF (frm_count > 0) AND frm_comp = '1' THEN
                    frm_count := frm_count - 1;
                END IF;

                IF frm_count >= 3 THEN
                    frm_count := 3;
                END IF;
                --std_logic_vector(to_unsigned(frm_count ,frm_count_sig'length));
                frm_count_sig <= std_logic_vector(to_unsigned(to_integer(unsigned(reg_arr(80))) * frm_count, frm_count_sig'length));
            END IF;
        END IF;
    END PROCESS;

    frame_proc : PROCESS (frm_count_sig, ready, lvds_clk_in, status(0), sys_rst) IS
        VARIABLE lines_to_read : INTEGER RANGE 0 TO 65535;
        VARIABLE stat_count : INTEGER RANGE 0 TO 12 := 0;
        VARIABLE frm_run, inte_blk, fot, inte_flag, inte_init : std_logic := '0';
        VARIABLE init_load : std_logic := '0';

    BEGIN
        IF sys_rst = '0' THEN
            load <= '0';
        ELSIF rising_edge(lvds_clk_in) THEN
            IF status(0) = '1' THEN
                IF stat_count = 12 - (2 * to_integer(unsigned(reg_arr(118)(1 DOWNTO 0)))) THEN
                    stat_count := 0;
                ELSE
                    lvds_stat <= status(stat_count);
                    stat_count := stat_count + 1;
                END IF;

            ELSE
                stat_count := 0;
            END IF;

            IF frm_count_sig = x"00000" THEN
                frm_comp <= '0';

            ELSIF frm_run = '0' AND inte_init = '0' THEN
                frm_comp <= '1';
                frm_run := '1';

            ELSE
                frm_comp <= '0';
                IF inte_blk = '0' AND reg_arr(70)(0) = '0' AND inte_flag = '0' THEN
                    inte_init := '1';
                END IF;

            END IF;

            IF inte_init = '1' THEN
                IF intrpt1 = '0' THEN --make this block independent
                    --delay1(15 downto 0) <= reg_arr(71);
                    --delay1(23 downto 16) <= reg_arr(72)(7 downto 0);
                    delay1 <= x"000EA6";
                    del_strt1 <= '1';
                    inte_flag := '1';
                ELSE
                    del_strt1 <= '0';
                    inte_blk := '1';
                    frm_comp <= '1';
                    inte_init := '0';
                    inte_flag := '0';
                    frm_run := '1';
                END IF;
            END IF;

            IF frm_run = '1' THEN
                IF init_load = '0' THEN --load buffer
                    load <= '1';
                    init_load := '1';
                ELSE
                    load <= '0';
                END IF;
                IF fot = '0' THEN --frame overhead time
                    IF intrpt2 = '0' THEN
                        delay2 <= x"000EA6";
                        del_strt2 <= '1';
                        status(3) <= '1';
                    ELSE
                        del_strt2 <= '0';
                        status(3) <= '0';
                        lines_to_read := to_integer(unsigned(reg_arr(1)));
                        fot := '1';
                    END IF;

                ELSIF status(1) = '0' AND load_run = '0' THEN --row transfer complete

                    IF lines_to_read < 1 THEN
                        fot := '0';

                        IF inte_blk = '0' THEN
                            frm_run := '0';
                        ELSE
                            init_load := '0';
                            inte_blk := '0';
                        END IF;

                    ELSE
                        init_load := '0';
                        IF reg_arr(68)(1) = '1' OR reg_arr(68)(2) = '1' THEN
                            lines_to_read := lines_to_read - 3073; --4
                        ELSE
                            lines_to_read := lines_to_read - 3073; --2
                        END IF;
                    END IF;
                END IF;

            END IF;
            status(4) <= inte_blk;
            status(5) <= inte_blk;
            status(2) <= fot AND frm_run;
        END IF;
    END PROCESS;
    spi_proc : PROCESS (data, spi_rd, spi_wr, addr) IS
        VARIABLE reserve : std_logic := '0';
    BEGIN

        IF rising_edge(spi_wr) THEN

            IF wr_spi_mask(to_integer(unsigned(addr))) = '0' THEN
                reg_arr(to_integer(unsigned(addr))) <= data;
            END IF;

        END IF;

    END PROCESS;
END Behavioral;----------------------------------------------------------------------------
--  SEQUENCER.vhd
--
--  This program is free software: you can redistribute it and/or
--  modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation, either version
--  2 of the License, or (at your option) any later version.
--
----------------------------------------------------------------------------




library IEEE;
use IEEE.STD_LOGIC_1164.all;
use ieee.numeric_std.all;
use IEEE.std_logic_unsigned.all;

entity SEQUENCER is
    generic (
        rd_spi_mask : std_logic_vector(127 downto 0) := x"00000000000000000000000000000000";
        wr_spi_mask : std_logic_vector(127 downto 0) := x"00000000000000000000000000000000"
    );

    port (
        ind_clk : in std_logic;

        rm_addr : out std_logic_vector(12 downto 0);
        rd_ram  : out std_logic;
        wr_buf  : in std_logic;
        r_data  : in std_logic_vector(11 downto 0);

        lvds_clk_in : in std_logic;
        frame_req   : in std_logic;
        txp1, txp2  : in std_logic;
        sys_rst     : in std_logic;

        spi_en, spi_in, spi_clk : in std_logic;
        spi_out : out std_logic;

        lvds_out     : out std_logic_vector(63 downto 0);
        lvds_clk_out : out std_logic;
        lvds_stat    : out std_logic
    );

end SEQUENCER;

architecture Behavioral of SEQUENCER is
    type memory is array (0 to 127) of std_logic_vector(15 downto 0);
    signal reg_arr : memory := (
        1 => x"0C00",
        67 => x"0001",
        68 => x"0009",
        71 => x"0600",
        73 => x"0600",
        79 => x"0001",
        80 => x"0001",
        82 => x"1632",
        83 => x"1705",
        84 => x"0082",
        85 => x"0082",
        86 => x"0082",
        87 => x"030C",
        88 => x"030C",
        89 => x"0055",
        90 => x"FFFF",
        91 => x"FFFF",
        92 => x"FFFF",
        93 => x"FFFF",
        94 => x"0007",
        95 => x"FFFF",
        96 => x"FFFF",
        98 => x"8888",
        99 => x"8888",
        102 => x"2040",
        103 => x"0FC0",
        104 => x"0040",
        105 => x"2040",
        106 => x"2040",
        107 => x"3060",
        108 => x"3060",
        109 => x"3060",
        110 => x"3060",
        111 => x"8888",
        113 => x"030A",
        114 => x"005F",
        116 => x"017F",
        117 => x"0004",
        118 => x"0001",
        120 => x"0009",
        121 => x"0001",
        122 => x"0020",
        124 => x"0005",
        125 => x"0002",
        126 => x"0302",
        others => x"0000"
    );

    signal start : std_logic;
    signal nxt_px_wire  : std_logic;
    signal mode_sr_md   : std_logic_vector(6 downto 0);
    signal shift_wire   : std_logic_vector(127 downto 0);
    signal px_data_wire : std_logic_vector(127 downto 0);
    signal load, ready  : std_logic;
    signal addr : std_logic_vector(6 downto 0);
    signal data : std_logic_vector(15 downto 0);
    signal spi_wr, spi_rd : std_logic;

    signal delay : std_logic_vector(23 downto 0);
    signal del_strt, intrpt : std_logic;

    signal delay1 : std_logic_vector(23 downto 0);
    signal del_strt1, intrpt1 : std_logic;

    signal frm_comp : std_logic;
    signal frm_count_sig : std_logic_vector(1 downto 0);

    signal status : std_logic_vector(11 downto 0);

    component SERIALIZER
        port (
            clk, rst, start, nxt_px : in std_logic;
            mode_sr  : in std_logic_vector(6 downto 0);
            dval     : out std_logic;
            lvds_out : out std_logic_vector(63 downto 0);
            shift    : out std_logic_vector(127 downto 0);
            pxdata   : in std_logic_vector(127 downto 0)
        );
    end component;

    component ROW_BUFFER
        port (
            clk, wr_buf, load : in std_logic;
            rd_ram, ready, nxt_px : out std_logic;
            bit_md  : in std_logic_vector(1 downto 0);
            data    : in std_logic_vector(11 downto 0);
            shift   : in std_logic_vector(127 downto 0);
            rm_addr : out std_logic_vector(12 downto 0);
            pxdata  : out std_logic_vector(127 downto 0)
        );
    end component;

    component SPI
        port (
            spi_en, spi_in, spi_clk : in std_logic;
            addr : out std_logic_vector(6 downto 0);
            data : inout std_logic_vector(15 downto 0);
            spi_out, rd, wr : out std_logic
        );
    end component;

begin
    status(6 downto 11) <= "01000";
    mode_sr_md <= "0000010";
    ser_map : SERIALIZER
    port map(
        clk => lvds_clk_in,
        rst => sys_rst,
        start => start,
        nxt_px => nxt_px_wire,
        mode_sr => mode_sr_md(6 downto 0),
        lvds_out => lvds_out,
        shift => shift_wire,
        dval => status(0),
        pxdata => px_data_wire
    );

    row_map : ROW_BUFFER
    port map(
        clk => lvds_clk_in,
        wr_buf => wr_buf,
        load => load,
        rd_ram => rd_ram,
        ready => ready,
        nxt_px => nxt_px_wire,
        bit_md => reg_arr(118)(1 downto 0),
        data => r_data,
        shift => shift_wire,
        rm_addr => rm_addr,
        pxdata => px_data_wire
    );

    spi_map : SPI
    port map(
        spi_en => spi_en,
        spi_in => spi_in,
        spi_clk => spi_clk,
        addr => addr,
        data => data,
        spi_out => spi_out,
        rd => spi_rd,
        wr => spi_wr
    );

    delay_proc : process (ind_clk, delay, del_strt) is
        variable count : std_logic_vector(23 downto 0) := x"FFFFFF";
        variable init : std_logic := '1';
    begin
        if rising_edge(ind_clk) then
            if del_strt = '1' and init = '1' then
                count := delay;
                init := '0';
                intrpt <= '0';

            elsif count = x"000000" and del_strt = '1' then
                intrpt <= '1';
                init := '1';

            elsif del_strt = '1' then
                count := count - 1;
                intrpt <= '0';
            else
                intrpt <= '0';
                init := '1';
            end if;
        end if;


    end process;

    delay1_proc : process (ind_clk, delay1, del_strt1) is
        variable count : std_logic_vector(23 downto 0) := x"FFFFFF";
        variable init  : std_logic := '1';
    begin
        if rising_edge(ind_clk) then
            if del_strt1 = '1' and init = '1' then
                count := delay1;
                init := '0';
                intrpt1 <= '0';

            elsif count = x"000000" and del_strt1 = '1' then
                intrpt1 <= '1';
                init := '1';

            elsif del_strt1 = '1' then
                count := count - 1;
                intrpt1 <= '0';
            else
                intrpt1 <= '0';
                init := '1';
            end if;
        end if;


    end process;

    main_proc : process (lvds_clk_in, frame_req, sys_rst, ready, intrpt, spi_en) is
        variable init1 : std_logic := '1';
        variable init2 : std_logic := '1';
        variable init3 : std_logic := '0';
        variable frm_count : integer range 0 to 3 := 0;

    begin
        if rising_edge(lvds_clk_in) then
            if init1 = '1' then
                if intrpt = '0' then
                    delay <= x"00FFFF";
                    del_strt <= '1';
                else
                    del_strt <= '0';
                    init1 := '0';
                end if;

            elsif sys_rst = '0' then
                frm_count := 0;
                init2 := '1';

            elsif init2 = '1' then
                if intrpt = '0' then
                    delay <= x"00FFFF";
                    del_strt <= '1';
                else
                    del_strt <= '0';
                    init2 := '0';
                end if;

            elsif spi_en = '1' then
                init3 := '1';

            elsif spi_en = '0' and init3 = '1' then
                if intrpt = '0' then
                    delay <= x"00FFFF";
                    del_strt <= '1';
                else
                    del_strt <= '0';
                    init3 := '0';
                end if;

            elsif spi_en = '0' then
                if frame_req = '1' then
                    frm_count := frm_count + 1;
                end if;

                if (frm_count > 0) and frm_comp = '1' then
                    frm_count := frm_count - 1;
                end if;

                if frm_count >= 3 then
                    frm_count := 3;
                end if;

                frm_count_sig <= std_logic_vector(to_unsigned(frm_count, frm_count_sig'length));
            end if;
        end if;
    end process;

    frame_proc : process (frm_count_sig, ready, lvds_clk_in, status(0), sys_rst) is
        variable lines_to_read : integer range 0 to 65535;
        variable frm_run, inte_blk, fot : std_logic := '0';

    begin
        if sys_rst = '0' then
            load <= '0';
        elsif rising_edge(lvds_clk_in) then
            if frm_count_sig = "00" then
                frm_comp <= '0';

            elsif frm_run = '0' then
                frm_comp <= '1';
                frm_run := '1';

            else
                frm_comp <= '0';
                if inte_blk = '0' and reg_arr(70)(0) = '0' then

                    if intrpt1 = '0' then
                        delay1(15 downto 0) <= reg_arr(71);
                        delay1(23 downto 16) <= reg_arr(72)(7 downto 0);
                        del_strt1 <= '1';
                    else
                        del_strt1 <= '0';
                        inte_blk := '1';
                        frm_comp <= '1';
                    end if;

                end if;
            end if;

            if frm_run = '1' then
                if fot = '0' then
                    if intrpt1 = '0' then
                        delay1 <= x"00FFFF";
                        del_strt1 <= '1';
                        load <= '1';
                        start <= '0';
                    else
                        load <= '0';
                        lines_to_read := to_integer(unsigned(reg_arr(1)));
                        fot := '1';
                    end if;

                elsif status(0) = '0' then

                    if lines_to_read = 0 then
                        if inte_blk = '0' then
                            frm_run := '0';
                        else
                            fot := '0';
                        end if;

                    else
                        fot := '0';
                        if reg_arr(68)(1) = '1' or reg_arr(68)(2) = '1' then
                            lines_to_read := lines_to_read - 4;
                        else
                            lines_to_read := lines_to_read - 2;
                        end if;

                        if ready = '1' then
                            start <= '1';
                        else
                            start <= '0';
                        end if;
                    end if;
                end if;

            end if;
        end if;
    end process;


    spi_proc : process (data, spi_rd, spi_wr, addr) is
        variable reserve : std_logic := '0';
    begin
        if rising_edge(spi_wr) then

            if wr_spi_mask(to_integer(unsigned(addr))) = '0' then
                reg_arr(to_integer(unsigned(addr))) <= data;
            end if;

        end if;

    end process;
end Behavioral;
