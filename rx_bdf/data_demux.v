module data_demux (
					input 		 clk,
					input [7:0]  select,
					input [15:0] data_in,
					input 		 data_strb,
					input        crc_valid,
					input 		 reset,
											output reg [15:0] data_0,
											output reg [15:0] data_1,
											output reg [15:0] data_2,
											output reg [15:0] data_3,
											output reg [15:0] data_4,
											output reg [15:0] data_5,
											output reg [15:0] data_6,
											output reg [15:0] data_7,
											output reg [15:0] data_8,
											output reg [15:0] data_9,
											output reg        receive_done
																			);

parameter [7:0] n_word = 8'h01;

localparam [7:0] max_wrd_indx = n_word - 8'h01;

(*ramstyle = "no_rw_check"*)reg [15:0] mem [max_wrd_indx:0];

reg [15:0] data_bf = 16'h0000;
reg [7:0]  n       = 8'h00;

reg pre_strb_0 = 1'b0;
reg pre_strb_1 = 1'b0;
reg valid_flg  = 1'b0;
reg first_word = 1'b0;


always @(posedge clk) begin
	pre_strb_0 <= data_strb;
	pre_strb_1 <= crc_valid;

	if(data_strb && !pre_strb_0) begin
		mem[select] <= data_in;
	end
	else begin
		if(valid_flg) begin
			if(!first_word) begin
				n <= 8'h00;
				data_bf <= mem[0];
				first_word <= 1'b1;
				if(!max_wrd_indx) begin
					valid_flg <= 1'b0;
					receive_done <= 1'b1;
				end
				else begin end
			end
			else begin
				data_bf <= mem[n + 8'h01];
				n <= n + 8'h01;
				if(n == max_wrd_indx - 8'h01) begin
					valid_flg <= 1'b0;
					receive_done <= 1'b1;	
				end
				else begin end
			end
		end
		else begin end
	end
	if(crc_valid && !pre_strb_1) begin
		valid_flg <= 1'b1;
		receive_done <= 1'b0;
		first_word <= 1'b0;	
	end
	else begin end

	if(reset) begin
		valid_flg <= 1'b0;
		first_word <= 1'b0;
		receive_done <= 1'b0;
	end
	else begin end
end

always @(posedge clk) begin
	if(reset) begin
		data_0 <= 16'h0000;
		data_1 <= 16'h0000;
		data_2 <= 16'h0000;
		data_3 <= 16'h0000;
		data_4 <= 16'h0000;
		data_5 <= 16'h0000;
		data_6 <= 16'h0000;
		data_7 <= 16'h0000;
		data_8 <= 16'h0000;
		data_9 <= 16'h0000;
	end
	else begin
		case(n)
			0: data_0 <= data_bf;
			1: data_1 <= data_bf;
			2: data_2 <= data_bf;
			3: data_3 <= data_bf;
			4: data_4 <= data_bf;
			5: data_5 <= data_bf;
			6: data_6 <= data_bf;
			7: data_7 <= data_bf;
			8: data_8 <= data_bf;
			9: data_9 <= data_bf;
			default begin end
		endcase
	end
end
endmodule 