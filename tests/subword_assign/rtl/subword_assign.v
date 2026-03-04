module subword_assign (
    input logic clk,
    input logic [8-1:0] subword_in,
    output logic [4-1:0] subword_out_1,
    output logic [4-1:0] subword_out_2
);

    assign subword_out_1 = subword_in[3:0];
    assign subword_out_2 = subword_in[7:4];

endmodule
