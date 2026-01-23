// Simple counter module for testing the parser
module counter(
    input wire clk,
    input wire reset,
    output reg [7:0] count
);
    
    always @(posedge clk) begin
        if (reset)
            count <= 8'b0;
        else
            count <= count + 1;
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
