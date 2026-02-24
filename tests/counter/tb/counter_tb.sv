`timescale 1ns/1ps

module sync_data_driver#(type T, string name)(
    input logic clk,
    output T data,
    output bit done
);
    localparam string base_dir = "../custom-sim/stimuli";

    function automatic void read_sync_file(string filepath, output T data[]);
        int fd;
        string line;
        data = new[0];
        fd = $fopen(filepath, "r");

        if (fd == 0) begin
          $display("ERROR: Could not open file: %s", filepath);
          $finish(1);
        end

        while (1) begin
          int value;
          line = "";

          // Read line, break on EOF
          if ($fgets(line, fd) == 0) break;

          // Skip empty lines
          if (line.len() == 0) continue;

          // Skip comments (line starting with '#')
          if (line.tolower().substr(0, 0) == "#") continue;

          // Parse "time value"
          if ($sscanf(line, "%d", value) == 1) begin
            data = new[data.size()+1](data);
            data[data.size()-1] = T'(value);
          end
          else begin
            $display("ERROR: In %s, could not parse line: %s", filepath, line);
            $finish(1);
          end
        end

        $fclose(fd);
    endfunction

    T signal_data[];
    int vector_index;

    string filepath = {base_dir, "/", name, ".txt"};

    initial done = 0;
    initial begin
        read_sync_file(filepath, signal_data);
        vector_index = 1;
        data = signal_data[0];
        $display("[%0t] Just set data INITIALLY to %0d", $realtime, data);
    end

    always @(posedge clk) begin
        if (vector_index < signal_data.size()) begin
            data <= signal_data[vector_index];
            vector_index <= vector_index + 1;
            $display("[%0t] Just set data to %0d", $realtime, data);
        end
        else begin
            done <= 1;
        end
    end

endmodule

module async_data_driver#(type T, string name)(
    output T data,
    output bit done
);
    localparam string base_dir = "../custom-sim/stimuli";

    typedef struct {
        int value;
        realtime timestamp;
    } async_type_t;

    function automatic void read_async_file(string filepath, output async_type_t data[]);
        int fd;
        string line;
        data = new[0];
        fd = $fopen(filepath, "r");

        if (fd == 0) begin
          $display("ERROR: Could not open file: %s", filepath);
          $finish;
        end

        while (1) begin
          int value;
          time timestamp;
          line = "";

          // Read line, break on EOF
          if ($fgets(line, fd) == 0) break;

          // Skip empty lines
          if (line.len() == 0) continue;

          // Skip comments (line starting with '#')
          if (line.tolower().substr(0, 0) == "#") continue;

          // Parse "time value"
          if ($sscanf(line, "%d %d", timestamp, value) == 2) begin
            data = new[data.size()+1](data);
            data[data.size()-1] = '{
              value: value,
              timestamp: timestamp
            };
          end
          else begin
            $display("WARNING: Could not parse line: %s", line);
          end
        end

        $fclose(fd);
    endfunction

    initial begin

        async_type_t signal_data[];

        string filepath = {base_dir, "/", name, ".txt"};

        read_async_file(filepath, signal_data);

        done = 0;

        foreach (signal_data[i]) begin
            $display("[%0t] Signal %s[%0d]: time=%0t(%0d) value=%0d", $realtime, name, i, signal_data[i].timestamp, int'(signal_data[i].timestamp), signal_data[i].value);
            #(signal_data[i].timestamp-$realtime) data = signal_data[i].value[0 +: $bits(T)];
        end
        done = 1;
    end
endmodule

module tb;
    localparam WIDTH = 8;

    logic clk;
    logic reset;
    logic a_rst;
    logic [WIDTH-1:0] count;
    logic done;

    counter#(.WIDTH(WIDTH)) dut (
        .clk(clk),
        .reset(reset),
        .a_rst(a_rst),
        .count(count),
        .done(done)
    );


    localparam DRIVERS = 3;
    bit [DRIVERS-1:0] dones;
    initial begin
        wait(&dones);
        $finish;
    end

    // Generate async signals.
    async_data_driver#(.T(logic), .name("clk")) u_clk_driver(.data(clk), .done(dones[0]));
    async_data_driver#(.T(logic), .name("a_rst")) u_a_rst_driver(.data(a_rst), .done(dones[1]));

    // Generate sync inputs.
    sync_data_driver#(.T(logic), .name("reset")) u_reset_driver(
        .clk(clk),
        .data(reset),
        .done(dones[2])
    );

    // Generate sync outputs.

    initial $timeformat(-9, -12, " ns", 10);

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
