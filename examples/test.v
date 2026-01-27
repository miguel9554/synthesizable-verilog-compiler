// Simple counter module for testing the parser
module counter(
    input wire clk,
    input logic reset,
    output reg [7:0] count
);

    wire [7:0] next_count;

    assign next_count = count +1;

    always @(posedge clk) begin
        if (reset)
            count <= 8'b0;
        else
            count <= next_count;
    end

endmodule

// Simple adder module
module adder(
    input wire [7:0] a,
    input wire [7:0] b,
    output wire [8:0] sum
);

    assign sum = a + b;

endmodule

`include "file.vh"
