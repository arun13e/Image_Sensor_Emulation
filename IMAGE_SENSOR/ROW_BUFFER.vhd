----------------------------------------------------------------------------
--  ROW_BUFFER.vhd
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

entity ROW_BUFFER is
    port (
        clk, wr_buf, load : in std_logic;                                       
        rd_ram, ready, nxt_px : out std_logic;                                  --ready indicator for Sequencer
        bit_md : in std_logic_vector(1 downto 0);                               --bit_md: 8/10/12 bit per pixel
        data : in std_logic_vector(11 downto 0);
        shift : in std_logic_vector(127 downto 0);                              --shift control lines
        rm_addr : out std_logic_vector(12 downto 0);                            --address for ram
        pxdata : out std_logic_vector(127 downto 0));                           --pixel data for serializer
end ROW_BUFFER;

architecture Behavioral of ROW_BUFFER is
    subtype row is std_logic_vector(11 downto 0);
    type mem is array (integer range 0 to 127, integer range 0 to 63) of row;
    signal row_var : mem;
    type sh_reg is array (integer range 0 to 127) of std_logic_vector(11 downto 0);
begin

    main_proc : process (clk, data, wr_buf, load, bit_md, shift) is
        variable shift_flag : std_logic := '0';
        variable shift_count : integer range 0 to 11 := 0;
        variable shift_reg : sh_reg;
        variable ld_flag, wr_flag, last_wr_flag : std_logic := '0';
        variable bit_ch : integer range 0 to 63 := 0;
        variable addr : std_logic_vector(12 downto 0) := (others => '0');
    begin

        if rising_edge(clk) then

            if load = '1' then                                                  --sets ld_flag, also serves as reset
                ready <= '0';
                ld_flag := '1';
                addr := (others => '0');
                nxt_px <= '0';
                bit_ch := 0;
                shift_count := 0;
                last_wr_flag := '0';
            end if;

            if ld_flag = '1' then                                               --stays in this control until buffer filled

                if wr_flag = '1' then                                           --buffer write cycle
                    rd_ram <= '0';
                    if wr_buf = '1' then

                        row_var((to_integer(unsigned(addr(12 downto 6)))), (to_integer(unsigned(addr(5 downto 0))))) <= data;
                        wr_flag := '0';
                        addr := addr + '1';                                     --next ram address to access
                    end if;

                elsif last_wr_flag = '1' then

                    for I in 0 to 127 loop                                      --updating shift register after last write
                        shift_reg(I) := row_var(I, bit_ch);
                    end loop;

                    ready <= '1';
                    ld_flag := '0';
                else
                    rd_ram <= '1';
                    rm_addr <= addr;
                    wr_flag := '1';

                    if addr = x"1FFF" then
                        last_wr_flag := '1';
                    end if;
                end if;
            else

                for I in 0 to 127 loop

                    if shift(I) = '1' then
                        shift_reg(I) := std_logic_vector(unsigned(shift_reg(I)) srl 1);
                        shift_flag := shift_flag or shift(I);                   --1 if even a single shift operation occurs
                    end if;

                    pxdata(I) <= shift_reg(I)(0);                               --assigning least significant bit to output
                end loop;

                if shift_flag = '1' then
                    shift_count := shift_count + 1;                             --increment counter if flag 1
                    shift_flag := '0';
                end if;

                case bit_md is
                    when "00" =>                                                --for 12 bit per pixel

                        if shift_count = 12 then
                            nxt_px <= '1';
                            bit_ch := bit_ch + 1;
                            shift_count := 0;

                            for I in 0 to 127 loop
                                shift_reg(I) := row_var(I, bit_ch);
                            end loop;

                        else
                            nxt_px <= '0';
                        end if;

                    when "01" =>                                                --for 10 bit per pixel

                        if shift_count = 10 then
                            nxt_px <= '1';
                            bit_ch := bit_ch + 1;
                            shift_count := 0;

                            for I in 0 to 127 loop
                                shift_reg(I) := row_var(I, bit_ch);
                            end loop;

                        else
                            nxt_px <= '0';
                        end if;

                    when "10" =>                                                --for 8 bit per pixel

                        if shift_count = 8 then
                            nxt_px <= '1';
                            bit_ch := bit_ch + 1;
                            shift_count := 0;

                            for I in 0 to 127 loop
                                shift_reg(I) := row_var(I, bit_ch);
                            end loop;

                        else
                            nxt_px <= '0';
                        end if;

                    when others =>
                end case;

            end if;
        end if;
    end process;
end Behavioral;
