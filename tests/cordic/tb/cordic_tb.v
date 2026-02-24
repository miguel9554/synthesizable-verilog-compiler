`timescale 1ns/1ps

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

    // ===== Clock Generation =====
    always begin
        #5 clk = ~clk;  // 100 MHz clock
    end

    // ===== Testbench Procedure =====
    initial begin
        // Initialize signals
        clk = 0;
        rst_n = 1;
        start = 0;
        angle_in = 16'sd0;
        
        // Reset the DUT
        rst_n = 0;
        #10 rst_n = 1;
        
        // Test Case 1: 0 degrees (angle_in = 0)
        angle_in = 16'sd0;  // 0 in Q1.14 format
        start = 1;
        #10 start = 0;
        
        // Wait for the calculation to finish
        wait(done);
        $display("Test 1: Angle = 0, cos = %d, sin = %d", cos_out, sin_out);

        // Test Case 2: 45 degrees (angle_in = 45 degrees ≈ 0.7854 rad)
        angle_in = 16'sb0011_0010_0100_0100;   // atan(2^-0) ≈ 45°;  // 45 degrees in Q1.14 format (about 0.7854 rad)
        #10 start = 1;
        #10 start = 0;
        
        // Wait for the calculation to finish
        wait(done);
        $display("Test 2: Angle = 45 degrees, cos = %d, sin = %d", cos_out, sin_out);
        
        // Test Case 3: -45 degrees (angle_in = -45 degrees ≈ -0.7854 rad)
        angle_in = -16'sb0011_0010_0100_0100;  // -45 degrees in Q1.14 format (about -0.7854 rad)
        #10 start = 1;
        #10 start = 0;
        
        // Wait for the calculation to finish
        wait(done);
        $display("Test 3: Angle = -45 degrees, cos = %d, sin = %d", cos_out, sin_out);

        // Test Case 4: 90 degrees (angle_in = 90 degrees ≈ 1.5708 rad)
        angle_in = 16'sb0110_0100_1000_1000;  // 90 degrees in Q1.14 format (about 1.5708 rad)
        #10 start = 1;
        #10 start = 0;
        
        // Wait for the calculation to finish
        wait(done);
        $display("Test 4: Angle = 90 degrees, cos = %d, sin = %d", cos_out, sin_out);
        
        // Test Case 5: -90 degrees (angle_in = -90 degrees ≈ -1.5708 rad)
        angle_in = -16'sb0110_0100_1000_1000;  // -90 degrees in Q1.14 format (about -1.5708 rad)
        #10 start = 1;
        #10 start = 0;
        
        // Wait for the calculation to finish
        wait(done);
        $display("Test 5: Angle = -90 degrees, cos = %d, sin = %d", cos_out, sin_out);

        // Test Case 6: 30 degrees (angle_in = 30 degrees ≈ 0.523621 rad)
        angle_in = 16'sb0010_0001_1000_0011;  // 30 degrees in Q1.14 format (about 0.523621 rad)
        #10 start = 1;
        #10 start = 0;
        
        // Wait for the calculation to finish
        wait(done);
        $display("Test 6: Angle = 30 degrees, cos = %d, sin = %d", cos_out, sin_out);

        // Test Case 7: -60 degrees (angle_in = -60 degrees ≈ -1.047180 rad)
        angle_in = 16'sb1011_1100_1111_1011;  // -60 degrees in Q1.14 format (about -1.047180 rad)
        #10 start = 1;
        #10 start = 0;
        wait(done);
        $display("Test 7: Angle = -60 degrees, cos = %d, sin = %d", cos_out, sin_out);

        // Test Case 8: 8.16 degrees (angle_in = 8.16 degrees ≈ 0.142395 rad)
        angle_in = 16'sb0000_1001_0001_1101;  // 8.16 degrees in Q1.14 format (about 0.142395 rad)
        #10 start = 1;
        #10 start = 0;
        wait(done);
        $display("Test 8: Angle = 8.16 degrees, cos = %d, sin = %d", cos_out, sin_out);

        // End the simulation
        #10
        $finish;
    end

endmodule
