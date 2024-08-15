module psram_top_apb (
    input         clock,
    input         reset,
    input  [31:0] in_paddr,
    input         in_psel,
    input         in_penable,
    input  [ 2:0] in_pprot,
    input         in_pwrite,
    input  [31:0] in_pwdata,
    input  [ 3:0] in_pstrb,
    output        in_pready,
    output [31:0] in_prdata,
    output        in_pslverr,

    output qspi_sck,
    output qspi_ce_n,
    inout [3:0] qspi_dio
);

  wire sck_temp;
  wire ce_n_temp;
  wire [3:0] din, dout, douten;
  wire ack;
  EF_PSRAM_CTRL_wb u0 (
      .clk_i(clock),
      .rst_i(reset),
      .adr_i(in_paddr),
      .dat_i(in_pwdata),
      .dat_o(in_prdata),
      .sel_i(in_pstrb),
      .cyc_i(in_psel),
      .stb_i(in_psel),
      .ack_o(ack),
      .we_i (in_pwrite),

      .sck(sck_temp),
      .ce_n(ce_n_temp),
      .din(din),
      .dout(dout),
      .douten(douten)
  );
  reg [4:0] counter;
  reg qpi_cmd;
  reg sck;
  reg ce_n;
  // qpi command counter
  always @(posedge clock) begin
    if (reset) begin
      counter <= 5'b0;
      sck <= 1'b0;
    end else begin
      counter <= counter + 1'b1;
      if (!ce_n) begin
        sck <= ~sck;
      end
    end
  end
  // state sign
  always @(posedge clock) begin
    if (reset) begin
      qpi_cmd <= 1'b1;
    end else begin
      if (counter == 5'd17) begin
        qpi_cmd <= 1'b0;
      end
    end
  end
  // ce_n logic
  always @(posedge clock) begin
    if (reset) begin
      ce_n <= 1'b1;
    end else begin
      if (counter == 5'd1) begin
        ce_n <= 1'b0;
      end
    end
  end
  wire [7:0] CMD_35H = 8'h35;

  assign in_pready = ack && in_psel;
  assign in_pslverr = 1'b0;
  assign qspi_dio[0] = qpi_cmd ? CMD_35H[8-counter[4:1]] : (douten[0] ? dout[0] : 1'bz);
  assign qspi_dio[1] = qpi_cmd ? 1'b0 : (douten[1] ? dout[1] : 1'bz);
  assign qspi_dio[2] = qpi_cmd ? 1'b0 : (douten[2] ? dout[2] : 1'bz);
  assign qspi_dio[3] = qpi_cmd ? 1'b0 : (douten[3] ? dout[3] : 1'bz);
  assign din = qspi_dio;
  assign qspi_sck = qpi_cmd ? sck : sck_temp;
  assign qspi_ce_n = qpi_cmd ? ce_n : ce_n_temp;

endmodule
