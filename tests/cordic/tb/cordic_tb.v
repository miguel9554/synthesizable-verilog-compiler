`timescale 1ns/1ps

module sync_recorder#(
    string filepath,
    type TYPE
)(
    input logic clk,
    input TYPE data
);
    // File handle
    integer file;

    // Open file at simulation start and write header
    initial begin
        file = $fopen(filepath, "w");
        if (file == 0) begin
            $display("ERROR: Cannot open file");
            $finish;
        end
        // Write header
        $fwrite(file, "# value at each clock\n");
    end

    // Whenever data changes, write to file
    always @(posedge clk) begin
        $fwrite(file, "%0d\n", data);
    end

    // Close file at end of simulation
    final begin
        $fclose(file);
    end

endmodule

module async_recorder#(
    string filepath
)(
    input logic data
);
    // Keep old value to detect changes
    logic old_data;

    // File handle
    integer file;

    // Open file at simulation start and write header
    initial begin
        file = $fopen(filepath, "w");
        if (file == 0) begin
            $display("ERROR: Cannot open file");
            $finish;
        end
        // Write header
        $fwrite(file, "# time value followed by value\n");

        // Initialize old_data and write initial value
        old_data = data;
        $fwrite(file, "%0t %0d\n", $realtime, data);
    end

    // Whenever data changes, write to file
    always @(data) begin
        // Only write if value actually changed
        if (data !== old_data) begin
            $fwrite(file, "%0t %0d\n", $realtime, data);
            old_data = data;
        end
    end

    // Close file at end of simulation
    final begin
        $fclose(file);
    end

endmodule

module cordic_recorder #(
    parameter WL = 16,      // Word length
    parameter FL = 14,      // Fractional length
    parameter N_ITER = 15   // Number of iterations
)(
    input  wire                clk,        // Clock signal
    input  wire                rst_n,      // Active-low reset
    input  wire                start,      // Start computation
    input  wire signed [WL-1:0] angle_in  // Input angle in radians, Q1.14, range: [-π/2, π/2]
);
    localparam string base_dir = "../custom-sim/stimuli";

    let path(name) = {base_dir, "/", name};

    async_recorder#(.filepath(path("clk.txt"))) u_clk_recorder(.data(clk));
    async_recorder#(.filepath(path("rst_n.txt"))) u_rst_recorder(.data(rst_n));

    sync_recorder#(.filepath(path("start.txt")), .TYPE(logic))
        u_start_recorder(.clk(clk), .data(start));
    sync_recorder#(.filepath(path("angle_in.txt")), .TYPE(logic signed [WL-1:0]))
        u_angle_in_recorder(.clk(clk), .data(angle_in));
endmodule

module tb;

    // ===== Parameter Definitions =====
    parameter WL = 16;    // Word length
    parameter FL = 14;    // Fraction length
    parameter N_ITER = 15;

    // ===== Testbench Signals =====
    reg clk;
    reg rst_n;
    reg start;
    reg  signed [WL-1:0] angle_in;  // Q1.14 格式，[-π/2, π/2]
    wire signed [WL-1:0] cos_out;   // Q1.14 格式
    wire signed [WL-1:0] sin_out;   // Q1.14 格式
    wire done;

    // ===== Instantiate the CORDIC Module =====
    cordic #(WL, FL, N_ITER) uut (
        .clk(clk),
        .rst_n(rst_n),
        .start(start),
        .angle_in(angle_in),
        .cos_out(cos_out),
        .sin_out(sin_out),
        .done(done)
    );

    cordic_recorder #(WL, FL, N_ITER) u_recorder (
        .clk(clk),
        .rst_n(rst_n),
        .start(start),
        .angle_in(angle_in)
    );

    // ===== Clock Generation =====
    always begin
        #5 clk = ~clk;  // 100 MHz clock
    end

    // ===== Testbench Procedure =====
    always @(posedge clk) begin
        // Initialize signals
        @(posedge clk) begin
            rst_n    <= 1;
            start    <= 0;
            angle_in <= 16'sd0;
        end

        // Reset the DUT
        @(posedge clk) begin
            rst_n <= 0;
        end

        repeat (2) @(posedge clk);

        @(posedge clk) begin
            rst_n <= 1;
        end

        // -------------------------------------------------
        // Test Case 1: 0 degrees
        @(posedge clk) begin
            angle_in <= 16'sd0;
            start    <= 1;
        end

        @(posedge clk) begin
            start <= 0;
        end

        do @(posedge clk); while (done != 1);
        $display("Test 1: Angle = 0, cos = %d, sin = %d", cos_out, sin_out);

        // -------------------------------------------------
        // Test Case 2: 45 degrees
        @(posedge clk) begin
            angle_in <= 16'sb0011_0010_0100_0100;
            start    <= 1;
        end

        @(posedge clk) begin
            start <= 0;
        end

        do @(posedge clk); while (done != 1);
        $display("Test 2: Angle = 45 degrees, cos = %d, sin = %d", cos_out, sin_out);

        // -------------------------------------------------
        // Test Case 3: -45 degrees
        @(posedge clk) begin
            angle_in <= -16'sb0011_0010_0100_0100;
            start    <= 1;
        end

        @(posedge clk) begin
            start <= 0;
        end

        do @(posedge clk); while (done != 1);
        $display("Test 3: Angle = -45 degrees, cos = %d, sin = %d", cos_out, sin_out);

        // -------------------------------------------------
        // Test Case 4: 90 degrees
        @(posedge clk) begin
            angle_in <= 16'sb0110_0100_1000_1000;
            start    <= 1;
        end

        @(posedge clk) begin
            start <= 0;
        end

        do @(posedge clk); while (done != 1);
        $display("Test 4: Angle = 90 degrees, cos = %d, sin = %d", cos_out, sin_out);

        // -------------------------------------------------
        // Test Case 5: -90 degrees
        @(posedge clk) begin
            angle_in <= -16'sb0110_0100_1000_1000;
            start    <= 1;
        end

        @(posedge clk) begin
            start <= 0;
        end

        do @(posedge clk); while (done != 1);
        $display("Test 5: Angle = -90 degrees, cos = %d, sin = %d", cos_out, sin_out);

        // -------------------------------------------------
        // Test Case 6: 30 degrees
        @(posedge clk) begin
            angle_in <= 16'sb0010_0001_1000_0011;
            start    <= 1;
        end

        @(posedge clk) begin
            start <= 0;
        end

        do @(posedge clk); while (done != 1);
        $display("Test 6: Angle = 30 degrees, cos = %d, sin = %d", cos_out, sin_out);

        // -------------------------------------------------
        // Test Case 7: -60 degrees
        @(posedge clk) begin
            angle_in <= 16'sb1011_1100_1111_1011;
            start    <= 1;
        end

        @(posedge clk) begin
            start <= 0;
        end

        do @(posedge clk); while (done != 1);
        $display("Test 7: Angle = -60 degrees, cos = %d, sin = %d", cos_out, sin_out);

        // -------------------------------------------------
        // Test Case 8: 8.16 degrees
        @(posedge clk) begin
            angle_in <= 16'sb0000_1001_0001_1101;
            start    <= 1;
        end

        @(posedge clk) begin
            start <= 0;
        end

        do @(posedge clk); while (done != 1);
        $display("Test 8: Angle = 8.16 degrees, cos = %d, sin = %d", cos_out, sin_out);

        repeat (2) @(posedge clk);
        $finish;
    end

    // Waveform dump
    initial begin
        string database_name;
        if (!$value$plusargs("WAVES=%s", database_name)) begin
            $fatal(1, "Please provide WAVES database name");
        end
        $dumpfile(database_name);
        $dumpvars(0, tb_top);
    end

endmodule
