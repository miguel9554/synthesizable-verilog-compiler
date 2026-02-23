`timescale 1ns/1ps

module async_data_driver#(type T, string name)(
    output T data,
    output bit done
);
    localparam string base_dir = "../../stimuli";

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


    bit[2-1:0] dones;
    initial begin
        wait(&dones);
        $finish;
    end

    async_data_driver#(.T(logic), .name("clk")) u_clk_driver(.data(clk), .done(dones[0]));
    async_data_driver#(.T(logic), .name("a_rst")) u_a_rst_driver(.data(a_rst), .done(dones[1]));

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
