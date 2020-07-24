module data_mux (
					input        clk,
					input        data_lock,
					input [7:0]  selector,
					input [15:0] data_0,
					input [15:0] data_1,
					input [15:0] data_2,
					input [15:0] data_3,
					input [15:0] data_4,
					input [15:0] data_5,
					input [15:0] data_6,
					input [15:0] data_7,
					input [15:0] data_8,
					input [15:0] data_9,
					input 		 reset,
												output reg [15:0] data_out
																				);

reg pre_strb = 1'b0;
																						
always @(posedge clk) begin
	pre_strb <= data_lock;
	
	if(reset)
		data_out <= 16'h0000;
	else begin
		if(data_lock && !pre_strb)
			case(selector)
				0: data_out <= data_0;
				1: data_out <= data_1;
				2: data_out <= data_2;
				3: data_out <= data_3;
				4: data_out <= data_4;
				5: data_out <= data_5;
				6: data_out <= data_6;
				7: data_out <= data_7;
				8: data_out <= data_8;
				9: data_out <= data_9;
				default: data_out <= data_out;
			endcase
		else begin end
	end
end
endmodule
