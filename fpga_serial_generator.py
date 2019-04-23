from jinja2 import Template

transmit_main_module = Template ("""
/*
	Developer: Sokolov R.V.
	Revision: 1.0

	Max clk freq: 600 MHz
	Max data rate: 150 Mbit/s ( Max clk freq / 4 )
	Data packet length [bit]: 32 + (20 * n_words) 

	If 'reset' port not use: connect 'reset' port to GND.
	If 'start' port not use: connect 'ready' port to 'start' port (for non-stop data transmit). 
	Start transmit: posedge signal on 'start' port.
	End transmit: posedge signal on 'ready' port.

	Add this file and all dependent files to project and
	create symbol from this file.

	Dependent files:
		- data_mux_{{prefix}}.v
		- serial_tx_ctrl_{{prefix}}.v
		- crc_16_rtu_tx_{{prefix}}.v
		- serial_transmitter_{{prefix}}.v
*/


module serial_transmitter_main_{{prefix}}(
	clk,
{%- for n in range(n_ports) %}
	data_{{n}},
{%- endfor %}
	start,
	reset,

	tx,
	ready
);


input wire	clk;
{%- for n in range(n_ports) %}
input wire	[15:0] data_{{n}};
{%- endfor %}
input wire	start;
input wire	reset;
output wire	tx;
output wire	ready;

wire	[7:0] byte_out;
reg	clk_dev_4;
wire	[15:0] crc_16;
wire	[7:0] data_delect;
wire	data_lock;
wire	main_reset;
wire	reset_crc;
wire	start_tx;
wire	transmit_done;
wire	tx_done;
wire	SYNTHESIZED_WIRE_0;
reg	SYNTHESIZED_WIRE_3;
wire	SYNTHESIZED_WIRE_1;
wire	[15:0] SYNTHESIZED_WIRE_2;





serial_transmitter_{{prefix}}	b2v_inst(
	.clk(clk_dev_4),
	.start(start_tx),
	.reset(main_reset),
	.byte_in(byte_out),
	.tx(tx),
	.ready(tx_done));


always@(posedge clk)
begin
	begin
	SYNTHESIZED_WIRE_3 <= SYNTHESIZED_WIRE_0;
	end
end


always@(posedge SYNTHESIZED_WIRE_3)
begin
	begin
	clk_dev_4 <= SYNTHESIZED_WIRE_1;
	end
end

assign	SYNTHESIZED_WIRE_1 =  ~clk_dev_4;

assign	SYNTHESIZED_WIRE_0 =  ~SYNTHESIZED_WIRE_3;


serial_tx_ctrl_{{prefix}}	b2v_inst5(
	.clk(clk_dev_4),
	.start(start),
	.tx_done(tx_done),
	.reset(main_reset),
	.crc_16(crc_16),
	.data_in(SYNTHESIZED_WIRE_2),
	.reset_crc(reset_crc),
	.start_tx(start_tx),
	.ready(transmit_done),
	.data_lock(data_lock),
	.byte_out(byte_out),
	.data_select(data_delect));
	defparam	b2v_inst5.n_word = {{n_ports}};


crc_16_rtu_tx_{{prefix}}	b2v_inst6(
	.clk(clk_dev_4),
	.start(start_tx),
	.reset(reset_crc),
	.byte_in(byte_out),
	
	.crc_16(crc_16));


data_mux_{{prefix}}	b2v_inst7(
	.clk(clk_dev_4),
	.data_lock(data_lock),
	.reset(main_reset),
{%- for n in range(n_ports) %}
	.data_{{n}}(data_{{n}}),
{%- endfor %}
	.selector(data_delect),
	.data_out(SYNTHESIZED_WIRE_2));

assign	main_reset = reset;
assign	ready = transmit_done;

endmodule
""")

data_mux_module = Template("""
module data_mux_{{prefix}} (
					input        clk,
					input        data_lock,
					input [7:0]  selector,
				{%- for n in range(n_ports) %}
					input [15:0] data_{{n}},
				{%- endfor %}
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
			{%- for n in range(n_ports) %}
				{{n}}: data_out <= data_{{n}};
			{%- endfor %}
				default: data_out <= data_out;
			endcase
		else begin end
	end
end
endmodule
""")

crc_16_rtu_tx_module = Template("""
module crc_16_rtu_tx_{{prefix}} (
				   	  input 	     clk,
				   	  input 	     start,
				   	  input [7:0] byte_in,
				   	  input       reset,
				   	  						     output reg [15:0] crc_16,
				   	  						     output reg        busy
																			);

localparam [15:0] polinom          = 16'hA001;

localparam [3:0]  IDLE             =  4'b0000;
localparam [3:0]  STAGE_0          =  4'b0001;
localparam [3:0]  STAGE_1          =  4'b0010;
localparam [3:0]  STAGE_2          =  4'b0011;
localparam [3:0]  STAGE_3          =  4'b0100;
localparam [3:0]  STAGE_4          =  4'b0101;
localparam [3:0]  STAGE_5          =  4'b0110;
localparam [3:0]  STAGE_6          =  4'b0111;
localparam [3:0]  STAGE_7          =  4'b1000;

	   reg [3:0]  state            =  4'b0000;

	   reg [15:0] crc              = 16'hFFFF;
	   reg        previous_strb    =  1'b0;

always @(posedge clk) begin
	previous_strb <= start;
	
	if(reset) begin
		crc <= 16'hFFFF;
		state <= IDLE;
		busy <= 1'b0;
	end
	else begin
		
		case(state)

			IDLE: begin
				if(start && !previous_strb) begin
					state <= STAGE_0;
					crc[7:0] <= crc[7:0] ^ byte_in[7:0];
					busy <= 1'b1;
				end
				else begin
					state <= IDLE;
				end
			end

			STAGE_0: begin
				if(crc & 16'h0001)
					crc <= (crc >> 16'h0001) ^ polinom;
				else
					crc <= crc >> 16'h0001;

				state <= STAGE_1;
			end

			STAGE_1: begin
				if(crc & 16'h0001)
					crc <= (crc >> 16'h0001) ^ polinom;
				else
					crc <= crc >> 16'h0001;

				state <= STAGE_2;
			end

			STAGE_2: begin
				if(crc & 16'h0001)
					crc <= (crc >> 16'h0001) ^ polinom;
				else
					crc <= crc >> 16'h0001;

				state <= STAGE_3;
			end

			STAGE_3: begin
				if(crc & 16'h0001)
					crc <= (crc >> 16'h0001) ^ polinom;
				else
					crc <= crc >> 16'h0001;

				state <= STAGE_4;
			end

			STAGE_4: begin
				if(crc & 16'h0001)
					crc <= (crc >> 16'h0001) ^ polinom;
				else
					crc <= crc >> 16'h0001;

				state <= STAGE_5;
			end

			STAGE_5: begin
				if(crc & 16'h0001)
					crc <= (crc >> 16'h0001) ^ polinom;
				else
					crc <= crc >> 16'h0001;

				state <= STAGE_6;
			end

			STAGE_6: begin
				if(crc & 16'h0001)
					crc <= (crc >> 16'h0001) ^ polinom;
				else
					crc <= crc >> 16'h0001;

				state <= STAGE_7;
			end

			STAGE_7: begin
				if(crc & 16'h0001) begin
					crc <= (crc >> 16'h0001) ^ polinom;
					crc_16 <= (crc >> 16'h0001) ^ polinom;
				end
				else begin
					crc <= crc >> 16'h0001;
					crc_16 <= crc >> 16'h0001;
				end
				
				busy <= 1'b0;
				state <= IDLE;
			end
			
			default begin
				state <= IDLE;
			end
			
		endcase
	end
end

endmodule
""")

serial_transmitter_module = Template("""
module serial_transmitter_{{prefix}} (clk, byte_in, start, reset, tx, ready);

input 		clk, start, reset;
input [7:0] byte_in;

output reg  tx, ready;

localparam [3:0] IDLE = 4'b0000;
localparam [3:0] ST_0 = 4'b0001;
localparam [3:0] ST_1 = 4'b0010;
localparam [3:0] ST_2 = 4'b0011;
localparam [3:0] ST_3 = 4'b0100;
localparam [3:0] ST_4 = 4'b0101;
localparam [3:0] ST_5 = 4'b0110;
localparam [3:0] ST_6 = 4'b0111;
localparam [3:0] ST_7 = 4'b1000;
localparam [3:0] STOP = 4'b1001;

reg [3:0] state     = 4'b0000;
reg [7:0] byte_bf   = 8'h00;

reg pre_strb 	  = 1'b0;

always @(posedge clk) begin
	pre_strb <= start;
	
	if(reset) begin
		state <= IDLE;
		tx <= 1'b1;
		ready <= 1'b1;
	end
	else begin
		case(state)
			IDLE: begin
				if(start && !pre_strb) begin
					tx <= 1'b0;	
					state <= ST_0;
					byte_bf <= byte_in;
					ready <= 1'b0;
				end
				else begin
					tx <= 1'b1;
					ready <= 1'b1;
				end
			end
			ST_0: begin tx <= byte_bf[0]; state <= ST_1; end
			ST_1: begin tx <= byte_bf[1]; state <= ST_2; end
			ST_2: begin tx <= byte_bf[2]; state <= ST_3; end
			ST_3: begin tx <= byte_bf[3]; state <= ST_4; end
			ST_4: begin tx <= byte_bf[4]; state <= ST_5; end
			ST_5: begin tx <= byte_bf[5]; state <= ST_6; end
			ST_6: begin tx <= byte_bf[6]; state <= ST_7; end
			ST_7: begin tx <= byte_bf[7]; state <= STOP; ready <= 1'b1; end
			STOP: begin tx <= 1'b1; state <= IDLE;  end
			default: begin 
				tx <= 1'b0;
				ready <= 1'b0;
				state <= IDLE;  
			end
		endcase
	end
end
endmodule
""")

serial_tx_ctrl_module = Template("""
module serial_tx_ctrl_{{prefix}} ( clk, data_in, start, tx_done, crc_16, reset,
						byte_out, reset_crc, start_tx, ready, data_select, data_lock );

input        clk, start, tx_done, reset;
input [15:0] data_in, crc_16;

output reg [7:0] byte_out, data_select;
output reg       start_tx, ready, data_lock, reset_crc;

parameter [7:0] n_word = 8'h01;

localparam [2:0] IDLE      = 3'b000;
localparam [2:0] DELAY     = 3'b001;
localparam [2:0] FST_BYTE  = 3'b010;
localparam [2:0] SD_HI     = 3'b011;
localparam [2:0] SD_LO     = 3'b100;
localparam [2:0] SD_CRC_HI = 3'b101;
localparam [2:0] SD_CRC_LO = 3'b110;

reg [2:0]  state     =  3'b000;
reg [2:0]  delay_cnt =  3'b000;

reg pre_strb_1 = 1'b0;
reg fst_flg    = 1'b0;

always @(posedge clk) begin
	pre_strb_1 <= tx_done;
	
	if(reset) begin
		state <= IDLE;
		reset_crc <= 1'b1;
		data_select <= 8'h00;
		ready <= 1'b0;
		start_tx <= 1'b0;	
	end
	else begin	
		case(state)
			IDLE: begin
				if(start) begin
					ready <= 1'b0;
					data_lock <= 1'b1;
					reset_crc <= 1'b0;
					state <= DELAY;
				end
				else begin
					ready <= 1'b1;
					data_lock <= 1'b0;
					state <= IDLE;
				end
			end		
			DELAY: begin
				fst_flg <= 1'b0;
				
				if(&delay_cnt) begin
					delay_cnt <= 3'b000;
					state <= FST_BYTE;
				end
				else begin
					delay_cnt <= delay_cnt + 3'b001;
					state <= DELAY;
				end
			end	
			FST_BYTE: begin
				byte_out <= data_in[15:8];
				fst_flg <= 1'b1;
				if(fst_flg)
					start_tx <= 1'b0;
				else
					start_tx <= 1'b1;

				if(tx_done && !pre_strb_1) begin
					data_select <= data_select + 8'h01;
					data_lock <= 1'b1;
					byte_out <= data_in[7:0];
					start_tx <= 1'b1;
					state <= SD_LO;
				end
				else begin
					data_lock <= 1'b0;
					state <= FST_BYTE;
				end
			end	
			SD_HI: begin			
				if(tx_done && !pre_strb_1) begin
					data_select <= data_select + 8'h01;
					data_lock <= 1'b1;
					byte_out <= data_in[7:0];
					start_tx <= 1'b1;
					state <= SD_LO;
				end
				else begin
					start_tx <= 1'b0;
					data_lock <= 1'b0;
					state <= SD_HI;
				end
			end
			SD_LO: begin	
				if(tx_done && !pre_strb_1) begin	
					start_tx <= 1'b1;
					data_lock <= 1'b0;
					if(data_select == n_word) begin
						data_select <= 8'h00;
						byte_out <= crc_16[15:8];
						reset_crc <= 1'b1;
						state <= SD_CRC_HI;
					end
					else begin	
						byte_out <= data_in[15:8];
						state <= SD_HI;
					end
				end
				else begin
					start_tx <= 1'b0;
					state <= SD_LO;
				end
			end
			SD_CRC_HI: begin
				start_tx <= 1'b0;
				
				if(tx_done && !pre_strb_1) begin
					start_tx <= 1'b1;
					byte_out <= crc_16[7:0];
					state <= SD_CRC_LO;
				end
				else begin
					state <= SD_CRC_HI;
				end
			end
			SD_CRC_LO: begin
				start_tx <= 1'b0;
				if(tx_done && !pre_strb_1) begin
					state <= IDLE;
					ready <= 1'b1;
				end
				else begin
					state <= SD_CRC_LO;
				end
			end
			
			default: begin
				state <= IDLE;
			end	
		endcase
	end
end
endmodule
""")

receiver_main_module = Template("""
/*
	Developer: Sokolov R.V.
	Revision: 1.0

	Max clk freq: 600 MHz
	Max data rate: 150 Mbit/s ( Max clk freq / 4 )
	Data packet length [bit]: 32 + (20 * n_words) 

	If 'reset' port not use: connect 'reset' port to GND.
	End receive: posedge signal on 'receive_done' port.

	Add this file and all dependent files to project and
	create symbol from this file.

	Dependent files:
		- data_demux_{{prefix}}.v
		- serial_rx_ctrl_{{prefix}}.v
		- crc_16_rtu_rx_{{prefix}}.v
		- serial_receiver_{{prefix}}.v
*/

module serial_receiver_main_{{prefix}}(
	clk,
	rx,
	reset,
{%- for n in range(n_ports) %}
	data_{{n}},
{%- endfor %}
	receive_done,
	errors_count
);

input wire	clk;
input wire	rx;
input wire	reset;
{%- for n in range(n_ports) %}
output wire	[15:0] data_{{n}};
{%- endfor %}

output wire	receive_done;
output wire	[15:0] errors_count;

wire	[7:0] byte_out;
reg	clk_dev_4;
wire	[15:0] crc_16;
wire	crc_busy;
wire	crc_reset;
wire	crc_valid;
wire	[7:0] d_select;
wire	d_strb;
wire	[15:0] data;
wire	[15:0] err_cnt;
wire	main_reset;
wire	ready_rx;
reg	rx_syn;
wire	tmout;
reg	DFF_inst1;
wire	SYNTHESIZED_WIRE_0;
reg	SYNTHESIZED_WIRE_2;
wire	SYNTHESIZED_WIRE_1;





serial_receiver_{{prefix}}	b2v_inst(
	.clk(clk),
	.rx(rx_syn),
	.ready(ready_rx),
	.timeout(tmout),
	.byte_out(byte_out));


always@(posedge clk)
begin
	begin
	DFF_inst1 <= rx;
	end
end


always@(posedge clk)
begin
	begin
	rx_syn <= DFF_inst1;
	end
end


always@(posedge clk)
begin
	begin
	SYNTHESIZED_WIRE_2 <= SYNTHESIZED_WIRE_0;
	end
end


always@(posedge SYNTHESIZED_WIRE_2)
begin
	begin
	clk_dev_4 <= SYNTHESIZED_WIRE_1;
	end
end

assign	SYNTHESIZED_WIRE_1 =  ~clk_dev_4;

assign	SYNTHESIZED_WIRE_0 =  ~SYNTHESIZED_WIRE_2;


serial_rx_ctrl_{{prefix}}	b2v_inst7(
	.clk(clk_dev_4),
	.rx_done(ready_rx),
	.tmout(tmout),
	.crc_busy(crc_busy),
	.reset(main_reset),
	.byte_in(byte_out),
	.crc_16(crc_16),
	.data_strb(d_strb),
	.validate(crc_valid),
	.crc_reset(crc_reset),
	.data_out(data),
	.errors_cnt(err_cnt),
	.selector(d_select));
	defparam	b2v_inst7.n_word = {{n_ports}};


crc_16_rtu_rx_{{prefix}}	b2v_inst8(
	.clk(clk_dev_4),
	.start(ready_rx),
	.reset(crc_reset),
	.byte_in(byte_out),
	.busy(crc_busy),
	.crc_16(crc_16));


data_demux_{{prefix}}	b2v_inst9(
	.clk(clk_dev_4),
	.data_strb(d_strb),
	.crc_valid(crc_valid),
	.reset(main_reset),
	.data_in(data),
	.select(d_select),
{%- for n in range(n_ports) %}	
	.data_{{n}}(data_{{n}}),
{%- endfor %}
	.receive_done(receive_done));
	defparam	b2v_inst9.n_word = {{n_ports}};

assign	main_reset = reset;
assign	errors_count = err_cnt;

endmodule
""")

serial_receiver_module = Template("""
module serial_receiver_{{prefix}} (clk, rx, byte_out, ready, timeout);

input clk, rx;

output reg [7:0] byte_out;
output reg       ready, timeout;

localparam [3:0] IDLE      = 4'b0000;
localparam [3:0] START_BIT = 4'b0001;
localparam [3:0] BIT_0     = 4'b0010;
localparam [3:0] BIT_1     = 4'b0011;
localparam [3:0] BIT_2     = 4'b0101;
localparam [3:0] BIT_3     = 4'b0110;
localparam [3:0] BIT_4     = 4'b0111;
localparam [3:0] BIT_5     = 4'b1000;
localparam [3:0] BIT_6     = 4'b1001;
localparam [3:0] BIT_7     = 4'b1010;
localparam [3:0] STOP_BIT  = 4'b1011;

reg [7:0] byte_bf        = 8'h00;
reg [3:0] state          = 4'b0000;
reg [2:0] delay_cnt_hi   = 3'b000;
reg [2:0] delay_cnt_lo   = 3'b000;
reg [1:0] data_strb_cnt  = 2'b01;

reg lo_cnt_done     = 1'b0;
reg recieve_flag    = 1'b0;

always @(posedge clk) begin

	delay_cnt_lo <= delay_cnt_lo + 3'b001;
	lo_cnt_done <= (delay_cnt_lo == 3'b110);
	if(lo_cnt_done)
		delay_cnt_hi <= delay_cnt_hi + 3'b001;
	else begin end
	if(!rx || timeout) begin
		delay_cnt_hi <= 3'b000;
		delay_cnt_lo <= 3'b000;
	end
	else begin end

	if(recieve_flag)
		data_strb_cnt <= data_strb_cnt + 2'b01;
	else
		data_strb_cnt <= 2'b00;

	if(!data_strb_cnt) begin
		case(state)
			IDLE: begin
				if(!rx) begin
					recieve_flag <= 1'b1;
					state <= START_BIT;
				end
				else begin
					recieve_flag <= 1'b0;
					state <= IDLE;
				end
			end
			START_BIT: begin  state <= BIT_0; end
			BIT_0: begin byte_bf[0] <= rx; state <= BIT_1; ready <= 1'b0; timeout <= 1'b0; end
			BIT_1: begin byte_bf[1] <= rx; state <= BIT_2; end
			BIT_2: begin byte_bf[2] <= rx; state <= BIT_3; end
			BIT_3: begin byte_bf[3] <= rx; state <= BIT_4; end
			BIT_4: begin byte_bf[4] <= rx; state <= BIT_5; end
			BIT_5: begin byte_bf[5] <= rx; state <= BIT_6; end
			BIT_6: begin byte_bf[6] <= rx; state <= BIT_7; end
			BIT_7: begin byte_bf[7] <= rx; state <= STOP_BIT; end
			STOP_BIT: begin
				byte_out <= byte_bf; 
				ready <= 1'b1;
				recieve_flag <= 1'b0; 
				state <= IDLE; 
			end
			default: begin end
		endcase
	end
	else begin end

	if(delay_cnt_hi == 3'b101) begin
		timeout <= 1'b1;
		recieve_flag <= 1'b0;
		state <= IDLE;
	end
	else begin end
		
end
endmodule
""")		

crc_16_rtu_rx_module = Template("""
module crc_16_rtu_rx_{{prefix}} (
				   	  input 	     clk,
				   	  input 	     start,
				   	  input [7:0] byte_in,
				   	  input       reset,
				   	  						     output reg [15:0] crc_16,
				   	  						     output reg        busy
																			);

localparam [15:0] polinom          = 16'hA001;

localparam [3:0]  IDLE             =  4'b0000;
localparam [3:0]  STAGE_0          =  4'b0001;
localparam [3:0]  STAGE_1          =  4'b0010;
localparam [3:0]  STAGE_2          =  4'b0011;
localparam [3:0]  STAGE_3          =  4'b0100;
localparam [3:0]  STAGE_4          =  4'b0101;
localparam [3:0]  STAGE_5          =  4'b0110;
localparam [3:0]  STAGE_6          =  4'b0111;
localparam [3:0]  STAGE_7          =  4'b1000;

	   reg [3:0]  state            =  4'b0000;

	   reg [15:0] crc              = 16'hFFFF;
	   reg        previous_strb    =  1'b0;
	   reg strb_bf = 1'b0;

always @(posedge clk) begin
	strb_bf <= start;
	previous_strb <= strb_bf;
	
	if(reset) begin
		crc <= 16'hFFFF;
		state <= IDLE;
		busy <= 1'b0;
	end
	else begin
		
		case(state)

			IDLE: begin
				if(strb_bf && !previous_strb) begin
					state <= STAGE_0;
					crc[7:0] <= crc[7:0] ^ byte_in[7:0];
					busy <= 1'b1;
				end
				else begin
					state <= IDLE;
				end
			end

			STAGE_0: begin
				if(crc & 16'h0001)
					crc <= (crc >> 16'h0001) ^ polinom;
				else
					crc <= crc >> 16'h0001;

				state <= STAGE_1;
			end

			STAGE_1: begin
				if(crc & 16'h0001)
					crc <= (crc >> 16'h0001) ^ polinom;
				else
					crc <= crc >> 16'h0001;

				state <= STAGE_2;
			end

			STAGE_2: begin
				if(crc & 16'h0001)
					crc <= (crc >> 16'h0001) ^ polinom;
				else
					crc <= crc >> 16'h0001;

				state <= STAGE_3;
			end

			STAGE_3: begin
				if(crc & 16'h0001)
					crc <= (crc >> 16'h0001) ^ polinom;
				else
					crc <= crc >> 16'h0001;

				state <= STAGE_4;
			end

			STAGE_4: begin
				if(crc & 16'h0001)
					crc <= (crc >> 16'h0001) ^ polinom;
				else
					crc <= crc >> 16'h0001;

				state <= STAGE_5;
			end

			STAGE_5: begin
				if(crc & 16'h0001)
					crc <= (crc >> 16'h0001) ^ polinom;
				else
					crc <= crc >> 16'h0001;

				state <= STAGE_6;
			end

			STAGE_6: begin
				if(crc & 16'h0001)
					crc <= (crc >> 16'h0001) ^ polinom;
				else
					crc <= crc >> 16'h0001;

				state <= STAGE_7;
			end

			STAGE_7: begin
				if(crc & 16'h0001) begin
					crc <= (crc >> 16'h0001) ^ polinom;
					crc_16 <= (crc >> 16'h0001) ^ polinom;
				end
				else begin
					crc <= crc >> 16'h0001;
					crc_16 <= crc >> 16'h0001;
				end
				
				busy <= 1'b0;
				state <= IDLE;
			end
			
			default begin
				state <= IDLE;
			end
			
		endcase
	end
end

endmodule			
""")		

serial_rx_ctrl_module = Template("""
module serial_rx_ctrl_{{prefix}} ( clk, rx_done, byte_in, tmout, crc_16, crc_busy, reset,
							data_out, selector, data_strb, validate, errors_cnt, crc_reset);

input clk, rx_done, tmout, crc_busy, reset;
input [7:0] byte_in;
input [15:0] crc_16;

output reg data_strb, validate, crc_reset;
output reg [7:0] selector;
output reg [15:0] data_out, errors_cnt;

parameter [7:0] n_word = 8'h01; 

localparam [7:0] max_wrd_indx = n_word - 8'h01;

localparam [2:0] IDLE    = 3'b000;
localparam [2:0] DATA_HI = 3'b001;
localparam [2:0] DATA_LO = 3'b010;
localparam [2:0] CRC_HI  = 3'b011;
localparam [2:0] CRC_LO  = 3'b100;
localparam [2:0] VALID   = 3'b101;

reg [2:0] state     = 3'b000;
reg [15:0] crc_hi_bf = 16'h0000;

reg pre_strb_0 = 1'b0;
reg fist_word  = 1'b0;
reg strb_bf    = 1'b0;

always @(posedge clk) begin
	strb_bf <= rx_done;
	pre_strb_0 <= strb_bf;

	if(reset) begin
		errors_cnt <= 16'h0000;
		state <= IDLE;
		validate <= 1'b0;
		data_strb <= 1'b0;
	end
	else begin
		case(state)
			IDLE: begin
				if(tmout) begin
					state <= DATA_HI;
					validate <= 1'b0;
				end
				else begin
					state <= IDLE;
				end
				fist_word <= 1'b0;
				selector <= 8'h00;
			end
			DATA_HI: begin
				if(!strb_bf)
					crc_reset <= 1'b0;
				else begin end
				data_strb <= 1'b0;
				if(strb_bf && !pre_strb_0) begin
					if(!fist_word) begin
						selector <= selector;
						fist_word <= 1'b1;
					end
					else begin
						selector <= selector + 8'h01;
					end
					data_out[15:8] <= byte_in;
					state <= DATA_LO;
				end

			end
			DATA_LO: begin
				if(strb_bf && !pre_strb_0) begin
					data_out[7:0] <= byte_in;
					data_strb <= 1'b1;
					if(selector == max_wrd_indx && fist_word) begin
						state <= CRC_HI;
					end
					else begin
						state <= DATA_HI;
					end
				end
			end
			CRC_HI: begin
				if(!crc_busy) begin
					crc_reset <= 1'b1;
				end
				else begin end
				if(strb_bf && !pre_strb_0) begin
					crc_hi_bf[15:8] <= byte_in;
					state <= CRC_LO;
				end
			end
			CRC_LO: begin
				data_strb <= 1'b0;
				if(strb_bf && !pre_strb_0) begin
					crc_hi_bf[7:0] <= byte_in;
					state <= VALID;
				end
			end
			VALID: begin
				if(crc_hi_bf == crc_16) begin
					validate <= 1'b1;
				end
				else begin
					errors_cnt <= errors_cnt + 16'h0001;
				end
				state <= IDLE;
			end
			default: state <= IDLE;
		endcase
	end
end
endmodule
""")

data_demux_module = Template("""
module data_demux_{{prefix}} (
					input 		 clk,
					input [7:0]  select,
					input [15:0] data_in,
					input 		 data_strb,
					input        crc_valid,
					input 		 reset,
										{%- for n in range(n_ports) %}
											output reg [15:0] data_{{n}}, 	
										{%- endfor %}
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
	{%- for n in range(n_ports) %}
		data_{{n}} <= 16'h0000;
	{%- endfor %}
	end
	else begin
		case(n)
		{%- for n in range(n_ports) %}
			{{n}}: data_{{n}} <= data_bf;
		{%- endfor %}	

			default: begin end
		endcase
	end
end
endmodule 
""")

print('\n ::: FPGA serial modules generator (Verylog) :::\n'
      ' ::: version: 1.0 :::\n '
      '::: developer: Sokolov R.V. :::\n')

n_ports_ask = input('Input count of words: ')
if not(n_ports_ask.isdigit()):
	n_ports_ask = 1
else:
	n_ports_ask = int(n_ports_ask)
	if n_ports_ask == 0:
		n_ports_ask =1

prefix_ask = input('Input prefix for your modules (example: if prefix is "'+str(n_ports_ask)+'_w", module name: \n 		- "serial_transmitter_main_'+str(n_ports_ask)+'_w"): ')

txt_out = transmit_main_module.render(
	prefix = prefix_ask,
	n_ports = n_ports_ask
	)
file_name = 'serial_transmitter_main_' + prefix_ask + '.v'
file = open(file_name, 'w')
file.write(txt_out)
file.close()

txt_out = serial_transmitter_module.render(
	prefix = prefix_ask,
	n_ports = n_ports_ask
	)
file_name = 'serial_transmitter_' + prefix_ask + '.v'
file = open(file_name, 'w')
file.write(txt_out)
file.close()

txt_out = crc_16_rtu_tx_module.render(
	prefix = prefix_ask,
	n_ports = n_ports_ask
	)
file_name = 'crc_16_rtu_tx_' + prefix_ask + '.v'
file = open(file_name, 'w')
file.write(txt_out)
file.close()

txt_out = data_mux_module.render(
	prefix = prefix_ask,
	n_ports = n_ports_ask
	)
file_name = 'data_mux_' + prefix_ask + '.v'
file = open(file_name, 'w')
file.write(txt_out)
file.close()

txt_out = serial_tx_ctrl_module.render(
	prefix = prefix_ask,
	n_ports = n_ports_ask
	)
file_name = 'serial_tx_ctrl_' + prefix_ask + '.v'
file = open(file_name, 'w')
file.write(txt_out)
file.close()



txt_out = receiver_main_module.render(
	prefix = prefix_ask,
	n_ports = n_ports_ask
	)
file_name = 'serial_receiver_main_' + prefix_ask + '.v'
file = open(file_name, 'w')
file.write(txt_out)
file.close()

txt_out = serial_receiver_module.render(
	prefix = prefix_ask,
	n_ports = n_ports_ask
	)
file_name = 'serial_receiver_' + prefix_ask + '.v'
file = open(file_name, 'w')
file.write(txt_out)
file.close()

txt_out = crc_16_rtu_rx_module.render(
	prefix = prefix_ask,
	n_ports = n_ports_ask
	)
file_name = 'crc_16_rtu_rx_' + prefix_ask + '.v'
file = open(file_name, 'w')
file.write(txt_out)
file.close()

txt_out = data_demux_module.render(
	prefix = prefix_ask,
	n_ports = n_ports_ask
	)
file_name = 'data_demux_' + prefix_ask + '.v'
file = open(file_name, 'w')
file.write(txt_out)
file.close()

txt_out = serial_rx_ctrl_module.render(
	prefix = prefix_ask,
	n_ports = n_ports_ask
	)
file_name = 'serial_rx_ctrl_' + prefix_ask + '.v'
file = open(file_name, 'w')
file.write(txt_out)
file.close()

print('\n >>> All files was generated in root folder!\n >>> Read the comments in the MAIN files! \n\n')
input(' >>> for exit push Enter ... <<<')