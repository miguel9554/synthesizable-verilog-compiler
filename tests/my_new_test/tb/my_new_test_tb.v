`timescale 1ns/1ps

module tb;
    reg clk;
    reg rst_n;

    my_new_test uut (
        .clk(clk),
        .rst_n(rst_n)
    );

    always begin
        #5 clk = ~clk;
    end

    initial begin
        clk = 0;
        rst_n = 0;
        #20 rst_n = 1;
        #100 $finish;
    end

    initial begin
        string database_name;
        if (!$value$plusargs("WAVES=%s", database_name)) begin
            $fatal(1, "Please provide WAVES database name");
        end
        $dumpfile(database_name);
        $dumpvars(0, tb);
    end
endmodule
