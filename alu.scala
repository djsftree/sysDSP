import spinal.core._
import spinal.sim._
import spinal.core.sim._
import scala.util.Random

// The code defines a wrapper for a DSP block, which is a black box. The wrapper takes in configuration 
// parameters for the DSP block and instantiates it with the given parameters. It also maps the clock 
// domain and adds an RTL path.

// The SysDSP_ALU class defines an ALU component that selects a DSP block from a memory based on a 
// selector input and uses it to perform arithmetic operations on input data. The configs memory stores 
// the different configurations for the DSP block, and the init method initializes the memory with the 
// default configurations. The ALU component selects a DSP block based on the sel input, and the 
// input data a, b, and c are fed into the selected DSP block via its input ports A, B, and C.
// The output M of the selected DSP block is then used to calculate the result of the arithmetic 
// operation and is stored in result.

// he code defines multiple SpinalEnum objects, which are used to define the different configuration 
// options for the DSP block, such as the multiplication mode, the ALU mode, the source mux mode, etc.

// In this particular implementation, the SysDSP_ALU component includes a cascade input and a cascade 
// output. The cascade input is connected to the cascadeOut output of the previous SysDSP_Wrapper 
// block, while the cascade output is connected to the cascadeIn input of the next SysDSP_Wrapper block.
// This allows the output of one SysDSP_Wrapper block to be used as an input to the next block in the chain.

// The cascadeIn_MUX and cascadeOut_MUX configuration parameters determine which inputs are connected 
// to the cascade input and cascade output, respectively. In this case, both parameters are set to
// SourceMuxMode.CASCADE_IN, indicating that the cascadeIn and cascadeOut signals should be used for
// the cascade input and output, respectively.

class SysDSP_Wrapper(config: SysDSPconfig) extends BlackBox {

  val generic = new Generic {
    val MULTMODE = config.multMode.value
    val ALUMODE = config.aluMode.value
    val ROUND = config.round.value
    val SOURCEA_MUX = config.sourceA_MUX.value
    val SOURCEB_MUX = config.sourceB_MUX.value
    val AMUX = config.amux.value
    val CASCADEIN_MUX = config.cascadeIn_MUX.value
    val CASCADEOUT_MUX = config.cascadeOut_MUX.value
    val AREG = config.AREG.value
    val BREG = config.BREG.value
    val CREG = config.CREG.value
    val MREG = config.MREG.value
    val SREG = config.SREG.value
    val SIGNED = config.signed.value
    val SATURATION_MODE = config.saturationMode.value
    val ROUNDING_MODE = config.roundingMode.value
  }

  val io = new Bundle {
    val A = in UInt (18 bits)
    val B = in UInt (18 bits)
    val C = in UInt (18 bits)
    val D = in UInt (18 bits)
    val ADD = in UInt (36 bits)
    val SRIA = in UInt (18 bits)
    val SRIB = in UInt (18 bits)
    val M = out UInt (36 bits)

    val AREG = in Bool ()
    val BREG = in Bool ()
    val CREG = in Bool ()
    val MREG = in Bool ()
    val SREG = in Bool ()
    val MODE = in UInt (2 bits)
    val ACC_MODE = in Bool ()
    
    val cascadeIn_MUX = in UInt(3 bits)
    val cascadeOut_MUX = in UInt(3 bits)
    val cascadeIn = in UInt(36 bits)
    val cascadeOut = out UInt(36 bits)

    val CE0 = in Bool ()
    val CE1 = in Bool ()
    val CE2 = in Bool ()
    val CE3 = in Bool ()

    val CLK0, CLK1, CLK2, CLK3 = in Clock ()
    val RST0, RST1, RST2, RST3 = in Reset ()

  }

  addGeneric("MULTMODE", generic.MULTMODE)
  addGeneric("ALUMODE", generic.ALUMODE)
  addGeneric("ROUND", generic.ROUND)
  addGeneric("SOURCEA_MUX", generic.SOURCEA_MUX)
  addGeneric("SOURCEB_MUX", generic.SOURCEB_MUX)
  addGeneric("AMUX", generic.AMUX)
  addGeneric("CASCADEIN_MUX", generic.CASCADEIN_MUX)
  addGeneric("CASCADEOUT_MUX", generic.CASCADEOUT_MUX)
  addGeneric("AREG", generic.AREG)
  addGeneric("BREG", generic.BREG)
  addGeneric("CREG", generic.CREG)
  addGeneric("MREG", generic.MREG)
  addGeneric("SREG", generic.SREG)
  addGeneric("SIGNED", generic.SIGNED)
  addGeneric("SATURATION_MODE", generic.SATURATION_MODE)
  addGeneric("ROUNDING_MODE", generic.ROUNDING_MODE)

  mapClockDomain(clockDomain)
  addRTLPath("path/to/your/SysDSP_Wrapper.v")
  noIoPrefix()
}

case class SysDSPconfig(
    name: String = "",
    multMode: SpinalEnumElement[SpinalEnumCraft] = MultMode.MULT18X18,
    aluMode: SpinalEnumElement[SpinalEnumCraft] = AluMode.ADD,
    round: SpinalEnumElement[SpinalEnumCraft] = RoundMode.TRUNC,
    sourceA_MUX: SpinalEnumElement[SpinalEnumCraft] = SourceMuxMode.A,
    sourceB_MUX: SpinalEnumElement[SpinalEnumCraft] = SourceMuxMode.B,
    amux: SpinalEnumElement[SpinalEnumCraft] = SourceMuxMode.ADD,
    cascadeIn_MUX: SpinalEnumElement[SpinalEnumCraft] = SourceMuxMode.CASCADE_IN,
    cascadeOut_MUX: SpinalEnumElement[SpinalEnumCraft] = SourceMuxMode.CASCADE_IN,
    AREG: Boolean = false,
    BREG: Boolean = false,
    CREG: Boolean = false,
    MREG: Boolean = false,
    SREG: Boolean = false,
    signed: Boolean = true,
    saturationMode: SpinalEnumElement[SpinalEnumCraft] = SaturationMode.NONE,
    roundingMode: SpinalEnumElement[SpinalEnumCraft] = RoundingMode.TRUNC
)

object MultMode extends SpinalEnum {
  val MULT18X18, MULT9X9, MULT27X18, MULT36X36 = newElement()
}

object AluMode extends SpinalEnum {
  val ADD, SUBTRACT, PASS_A, PASS_B, PASS_C = newElement()
}

object AluSum extends SpinalEnum(binarySequential) {
  val SUM, CARRYIN, CARRYINBAR = newElement()
}

object AluAccum extends SpinalEnum(binarySequential) {
  val ACCUM, DONTACCUM = newElement()
}

object AluRound extends SpinalEnum(binarySequential) {
  val ROUND, DONTROUND = newElement()
}

object SourceAMux extends SpinalEnum(binarySequential) {
  val ZERO, ONE, A, NOTA, B, NOTB, XOR, XNOR = newElement()
}

object SourceBMux extends SpinalEnum(binarySequential) {
  val B, ACC = newElement()
}

object AMux extends SpinalEnum(binarySequential) {
  val B, C = newElement()
}

object Cascade extends SpinalEnum(binarySequential) {
  val DISABLE, CASCADE = newElement()
}

object SourceAAdder extends SpinalEnum(binarySequential) {
  val A, NOTA, B, NOTB, ADD, SUB = newElement()
}

object SourceBAdder extends SpinalEnum(binarySequential) {
  val B, NOTB, ACC, ZERO, ADD, SUB = newElement()
}

object ProductWidth extends SpinalEnum(binarySequential) {
  val `18`, `19`, `20` = newElement()
}

object SignedMode extends SpinalEnum(binarySequential) {
  val UNSIGNED, SIGNED = newElement()
}

object CarryInSelect extends SpinalEnum(binarySequential) {
  val `0`, `1`, `CIN` = newElement()
}

object OperationMode extends SpinalEnum(binarySequential) {
  val MULTIPLY, MULTADDSUB, MULTADDSUBSUM, ALUMODE = newElement()
}

object AluMode extends SpinalEnum(binarySequential) {
  val ALUMODE, SHIFTA, SHIFTB, SHIFTAB = newElement()
}

object RoundMode extends SpinalEnum {
  val RNDN, TRUNC = newElement()
}

object AccumMode extends SpinalEnum(binarySequential) {
  val ACCUM, DONTACCUM = newElement()
}

object SumMode extends SpinalEnum(binarySequential) {
  val SUM, CARRYIN, CARRYINBAR = newElement()
}

object MultBypass extends SpinalEnum(binarySequential) {
  val DISABLED, ENABLED = newElement()
}

object ResetMode extends SpinalEnum(binarySequential) {
  val ASYNC, SYNC = newElement()
}

object GSR extends SpinalEnum(binarySequential) {
  val DISABLED, ENABLED = newElement()
}

object SourceMuxMode extends SpinalEnum {
  val A, B, C, D, ADD, SRIA, SRIB, CASCADE_IN = newElement()
}

object SaturationMode extends SpinalEnum {
  val NONE, OVERFLOW, SYMMETRIC = newElement()
}

object RoundingMode extends SpinalEnum {
  val TRUNC, ROUND_HALF_UP, ROUND_HALF_DOWN, ROUND_HALF_TO_EVEN = newElement()
}




class SysDSP_ALU extends Component {
  val io = new Bundle {
    val sel = in UInt (6 bits)
    val a = in UInt (32 bits)
    val b = in UInt (32 bits)
    val c = in UInt (32 bits)
    val result = out UInt (64 bits)
  }

  // Initialize the configs
  val configs = Mem(SysDSPconfig(), 32)

  configs.init(
    Seq(
      // Unsigned addition
      SysDSPconfig(
        name = "ADD_UNSIGNED",
        multMode = MultMode.MULT18X18,
        aluMode = AluMode.ADD,
        signed = false
      ),
      // Signed addition
      SysDSPconfig(
        name = "ADD_SIGNED",
        multMode = MultMode.MULT18X18,
        aluMode = AluMode.ADD,
        signed = true
      ),
      // Unsigned subtraction
      SysDSPconfig(
        name = "SUB_UNSIGNED",
        multMode = MultMode.MULT18X18,
        aluMode = AluMode.SUBTRACT,
        signed = false
      ),
      // Signed subtraction
      SysDSPconfig(
        name = "SUB_SIGNED",
        multMode = MultMode.MULT18X18,
        aluMode = AluMode.SUBTRACT,
        signed = true
      ),
      // AND operation
        SysDSPconfig (
          name = "AND",
          aluMode = AluMode.PASS_A,
          sourceA_MUX = SourceMuxMode.A,
          sourceB_MUX = SourceMuxMode.B
        ),
      // OR operation
      SysDSPconfig(
        name = "OR",
        aluMode = AluMode.PASS_A,
        sourceA_MUX = SourceMuxMode.NOTA,
        sourceB_MUX = SourceMuxMode.NOTB
      ),
      // NAND operation
      SysDSPconfig(
        name = "NAND",
        aluMode = AluMode.PASS_A,
        sourceA_MUX = SourceMuxMode.A,
        sourceB_MUX = SourceMuxMode.B,
        amux = SourceMuxMode.NOTB
      ),
      // NOR operation
      SysDSPconfig(
        name = "NOR",
        aluMode = AluMode.PASS_A,
        sourceA_MUX = SourceMuxMode.NOTA,
        sourceB_MUX = SourceMuxMode.NOTB,
        amux = SourceMuxMode.NOTB
      ),
      // XOR operation
      SysDSPconfig(
        name = "XOR",
        aluMode = AluMode.PASS_A,
        sourceA_MUX = SourceMuxMode.A,
        sourceB_MUX = SourceMuxMode.B,
        amux = SourceMuxMode.XOR
      ),
      // XNOR operation
      SysDSPconfig(
        name = "XNOR",
        aluMode = AluMode.PASS_A,
        sourceA_MUX = SourceMuxMode.A,
        sourceB_MUX = SourceMuxMode.B,
        amux = SourceMuxMode.XNOR
      ),
      SysDSPconfig(
        name = "MULT18X18_UNSIGNED",
        multMode = MultMode.MULT18X18,
        signed = false
      ),
      // Signed 18x18 multiplication
      SysDSPconfig(
        name = "MULT18X18_SIGNED",
        multMode = MultMode.MULT18X18,
        signed = true
      ),
      // Unsigned 9x9 multiplication
      SysDSPconfig(
        name = "MULT9X9_UNSIGNED",
        multMode = MultMode.MULT9X9,
        signed = false
      ),
      // Signed 9x9 multiplication
      SysDSPconfig(
        name = "MULT9X9_SIGNED",
        multMode = MultMode.MULT9X9,
        signed = true
      ),
      // Unsigned 27x18 multiplication
      SysDSPconfig(
        name = "MULT27X18_UNSIGNED",
        multMode = MultMode.MULT27X18,
        signed = false
      ),
      // Signed 27x18 multiplication
      SysDSPconfig(
        name = "MULT27X18_SIGNED",
        multMode = MultMode.MULT27X18,
        signed = true
      ),
      // Unsigned 36x36 multiplication
      SysDSPconfig(
        name = "MULT36X36_UNSIGNED",
        multMode = MultMode.MULT36X36,
        signed = false
      ),
      // Signed 36x36 multiplication
      SysDSPconfig(
        name = "MULT36X36_SIGNED",
        multMode = MultMode.MULT36X36,
        signed = true
      ),
      // Shift left A operation
      SysDSPconfig(
        name = "SHIFT_LEFT_A",
        aluMode = AluMode.SHIFTA,
        sourceA_MUX = SourceMuxMode.A,
        sourceB_MUX = SourceMuxMode.B
      ),
      // Shift right A operation
      SysDSPconfig(
        name = "SHIFT_RIGHT_A",
        aluMode = AluMode.SHIFTA,
        sourceA_MUX = SourceMuxMode.SRIA,
        sourceB_MUX = SourceMuxMode.B
      ),
      // Shift left B operation
      SysDSPconfig(
        name = "SHIFT_LEFT_B",
        aluMode = AluMode.SHIFTB,
        sourceA_MUX = SourceMuxMode.A,
        sourceB_MUX = SourceMuxMode.B
      ),
      // Shift right B operation
      SysDSPconfig(
        name = "SHIFT_RIGHT_B",
        aluMode = AluMode.SHIFTB,
        sourceA_MUX = SourceMuxMode.A,
        sourceB_MUX = SourceMuxMode.SRIB
      ),
      // Shift left A and B operation
      SysDSPconfig(
        name = "SHIFT_LEFT_AB",
        aluMode = AluMode.SHIFTAB,
        sourceA_MUX = SourceMuxMode.A,
        sourceB_MUX = SourceMuxMode.B
      ),
      // Shift right A and B operation
      SysDSPconfig(
        name = "SHIFT_RIGHT_AB",
        aluMode = AluMode.SHIFTAB,
        sourceA_MUX = SourceMuxMode.SRIA,
        sourceB_MUX = SourceMuxMode.SRIB
      )
    ).padTo(
      64,
      SysDSPconfig()
    ) // Pad the sequence with default configs if there are less than 64 entries
  )
  
  // First DSP block
  val dsp1 = new SysDSP_Wrapper(configs.read(io.sel))
  dsp1.io.A := io.a.asUInt
  dsp1.io.B := io.b.asUInt
  dsp1.io.CLK0 := clock
  dsp1.io.RST0 := reset

  // Second DSP block
  val dsp2 = new SysDSP_Wrapper(configs.read(io.sel))
  dsp2.io.A := dsp1.io.M(17 downto 0)
  dsp2.io.B := dsp1.io.M(35 downto 18)
  dsp2.io.CLK0 := clock
  dsp2.io.RST0 := reset
  
  dsp2.io.cascadeIn_MUX := Cascade.CASCADE
  dsp2.io.cascadeOut := dsp1.io.cascadeIn
  dsp2.io.cascadeOut := dsp.io.cascadeIn
  
  io.result := dsp2.io.M.asSInt
  
  when(reset) {
    dsp1.io.A := 0
    dsp1.io.B := 0
    dsp1.io.C := 0
    dsp2.io.A := 0
    dsp2.io.B := 0
    dsp2.io.C := 0
  }
}





class SysDSP_ALU_Testbench extends Component {
  
  val io = new Bundle {
    val a      = in Bits(32 bits)
    val b      = in Bits(32 bits)
    val c      = in Bits(32 bits)
    val sel    = in Bits(6 bits)
    val result = out Bits(64 bits)
  }

  val alu = new SysDSP_ALU
  
  alu.io.a   := io.a
  alu.io.b   := io.b
  alu.io.c   := io.c
  alu.io.sel := io.sel
  io.result  := alu.io.result

  def test(a: Int, b: Int, c: Int, sel: Int, expectedResult: Int): Unit = {
    
    val clockPeriod = 10
    val timeout = 1000
    val dut = SimConfig.withWave.compile(new SysDSP_ALU_Testbench)
    
    dut.doSimUntilVoid { dut =>
      
      dut.clockDomain.forkStimulus(period = clockPeriod)
      
      dut.io.a #= a
      dut.io.b #= b
      dut.io.c #= c
      dut.io.sel #= sel
      
      sleep(clockPeriod)
      val result = dut.io.result.toInt
      
      assert(result == expectedResult, s"Unexpected result. Expected $expectedResult, but got $result")
      
    }(timeout)
    
  }
  
}


object SysDSP_ALU_Test extends App {
  
  val tb = new SysDSP_ALU_Testbench

  // Test case 1: Add
  tb.test(0x12345678, 0x87654321, 0, 0, 0x99999999)

  // Test case 2: Subtract
  tb.test(0x12345678, 0x87654321, 0, 1, 0x8AC91357)

  // Test case 3: Multiply
  tb.test(0x1234ABCD, 0x5678DCBA, 0, 2, 0x1B851A50)

  // Test case 4: Multiply-Accumulate
  tb.test(0x1234ABCD, 0x5678DCBA, 0xCAFEBABE, 3, 0xEEFEBE0E)

  // Test case 5: Multiply-Accumulate with rounding

  ...
  
}
