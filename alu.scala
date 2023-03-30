import spinal.core._
import spinal.lib._

// This code defines two SpinalHDL components: SysDSP_Wrapper and SysDSP_Mult18_Operation.

// SysDSP_Wrapper is a black box that wraps an external System DSP block. It has a set of input and output signals, 
// and it uses the Generic class to define some configuration parameters. The io bundle defines the input and output
// signals, and they are mapped to the generic parameters using the addGeneric method. The mapClockDomain method 
// maps the clock domain of the SysDSP_Wrapper component to the current one. The addRTLPath method specifies the 
// path to the Verilog code of the wrapped block, and the noIoPrefix method disables prefixing the I/O signals 
// with the instance name.

// SysDSP_Mult18_Operation is a component that uses SysDSP_Wrapper to perform a multiplication of two 18-bit 
// signed integers. It has an io bundle that defines the input and output signals. It uses a Mem to store a set of 
// configuration parameters for SysDSP_Wrapper, and it selects one of them based on the sel input signal. 
// The selected configuration is used to set the generic parameters of SysDSP_Wrapper using the input signals 
// of SysDSP_Mult18_Operation. Finally, the result and cascadeOut output signals of SysDSP_Wrapper are converted 
// to SInt and connected to the output signals of SysDSP_Mult18_Operation.

class SysDSP_Wrapper(config: Mult18Config) extends BlackBox {

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

    val cascadeIn = in UInt (36 bits)
    val cascadeOut = out UInt (36 bits)

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

  mapClockDomain(clockDomain)
  addRTLPath("path/to/your/SysDSP_Wrapper.v")
  noIoPrefix()
}

case class Mult18Config(
    name: String = "",
    multMode: SpinalEnumElement[SpinalEnumCraft] = MultMode.MULT18X18,
    aluMode: SpinalEnumElement[SpinalEnumCraft] = AluMode.ADD,
    round: SpinalEnumElement[SpinalEnumCraft] = RoundMode.TRUNC,
    sourceA_MUX: SpinalEnumElement[SpinalEnumCraft] = SourceMuxMode.A,
    sourceB_MUX: SpinalEnumElement[SpinalEnumCraft] = SourceMuxMode.B,
    amux: SpinalEnumElement[SpinalEnumCraft] = SourceMuxMode.ADD,
    cascadeIn_MUX: SpinalEnumElement[SpinalEnumCraft] =
      SourceMuxMode.CASCADE_IN,
    cascadeOut_MUX: SpinalEnumElement[SpinalEnumCraft] =
      SourceMuxMode.CASCADE_IN,
    AREG: Boolean = false,
    BREG: Boolean = false,
    CREG: Boolean = false,
    MREG: Boolean = false,
    SREG: Boolean = false,
    signed: Boolean = true
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
  val DISABLED, CASCADE = newElement()
}

object SourceBMux extends SpinalEnum(binarySequential) {
  val B, ACC = newElement()
}

object AMux extends SpinalEnum(binarySequential) {
  val B, C = newElement()
}

object Cascade extends SpinalEnum(binarySequential) {
  val DISABLED, CASCADE = newElement()
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

import spinal.core._
import spinal.lib._

class SysDSP_Mult18_Operation extends Component {
  val io = new Bundle {
    val sel = in UInt (6 bits)
    val a = in SInt (18 bits)
    val b = in SInt (18 bits)
    val c = in SInt (18 bits)
    val cascadeIn = in SInt (54 bits)
    val result = out SInt (54 bits)
    val cascadeOut = out SInt (54 bits)
  }

  // Initialize the configs

  val configs = Mem(Mult18Config(), 32)

  configs.init(
    Seq(
      // Unsigned addition
      Mult18Config(
        name = "ADD_UNSIGNED",
        multMode = MultMode.MULT18X18,
        aluMode = AluMode.ADD,
        signed = false
      ),
      // Signed addition
      Mult18Config(
        name = "ADD_SIGNED",
        multMode = MultMode.MULT18X18,
        aluMode = AluMode.ADD,
        signed = true
      ),
      // Unsigned subtraction
      Mult18Config(
        name = "SUB_UNSIGNED",
        multMode = MultMode.MULT18X18,
        aluMode = AluMode.SUBTRACT,
        signed = false
      ),
      // Signed subtraction
      Mult18Config(
        name = "SUB_SIGNED",
        multMode = MultMode.MULT18X18,
        aluMode = AluMode.SUBTRACT,
        signed = true
      )
      // AND operation
        Mult18Config (
          name = "AND",
          aluMode = AluMode.PASS_A,
          sourceA_MUX = SourceMuxMode.A,
          sourceB_MUX = SourceMuxMode.B
        ),
      // OR operation
      Mult18Config(
        name = "OR",
        aluMode = AluMode.PASS_A,
        sourceA_MUX = SourceMuxMode.NOTA,
        sourceB_MUX = SourceMuxMode.NOTB
      ),
      // NAND operation
      Mult18Config(
        name = "NAND",
        aluMode = AluMode.PASS_A,
        sourceA_MUX = SourceMuxMode.A,
        sourceB_MUX = SourceMuxMode.B,
        amux = SourceMuxMode.NOTB
      ),
      // NOR operation
      Mult18Config(
        name = "NOR",
        aluMode = AluMode.PASS_A,
        sourceA_MUX = SourceMuxMode.NOTA,
        sourceB_MUX = SourceMuxMode.NOTB,
        amux = SourceMuxMode.NOTB
      ),
      // XOR operation
      Mult18Config(
        name = "XOR",
        aluMode = AluMode.PASS_A,
        sourceA_MUX = SourceMuxMode.A,
        sourceB_MUX = SourceMuxMode.B,
        amux = SourceMuxMode.XOR
      ),
      // XNOR operation
      Mult18Config(
        name = "XNOR",
        aluMode = AluMode.PASS_A,
        sourceA_MUX = SourceMuxMode.A,
        sourceB_MUX = SourceMuxMode.B,
        amux = SourceMuxMode.XNOR
      ),
      Mult18Config(
        name = "MULT18X18_UNSIGNED",
        multMode = MultMode.MULT18X18,
        signed = false
      ),
      // Signed 18x18 multiplication
      Mult18Config(
        name = "MULT18X18_SIGNED",
        multMode = MultMode.MULT18X18,
        signed = true
      ),
      // Unsigned 9x9 multiplication
      Mult18Config(
        name = "MULT9X9_UNSIGNED",
        multMode = MultMode.MULT9X9,
        signed = false
      ),
      // Signed 9x9 multiplication
      Mult18Config(
        name = "MULT9X9_SIGNED",
        multMode = MultMode.MULT9X9,
        signed = true
      ),
      // Unsigned 27x18 multiplication
      Mult18Config(
        name = "MULT27X18_UNSIGNED",
        multMode = MultMode.MULT27X18,
        signed = false
      ),
      // Signed 27x18 multiplication
      Mult18Config(
        name = "MULT27X18_SIGNED",
        multMode = MultMode.MULT27X18,
        signed = true
      ),
      // Unsigned 36x36 multiplication
      Mult18Config(
        name = "MULT36X36_UNSIGNED",
        multMode = MultMode.MULT36X36,
        signed = false
      ),
      // Signed 36x36 multiplication
      Mult18Config(
        name = "MULT36X36_SIGNED",
        multMode = MultMode.MULT36X36,
        signed = true
      ),
      // Shift left A operation
      Mult18Config(
        name = "SHIFT_LEFT_A",
        aluMode = AluMode.SHIFTA,
        sourceA_MUX = SourceMuxMode.A,
        sourceB_MUX = SourceMuxMode.B
      ),
      // Shift right A operation
      Mult18Config(
        name = "SHIFT_RIGHT_A",
        aluMode = AluMode.SHIFTA,
        sourceA_MUX = SourceMuxMode.SRIA,
        sourceB_MUX = SourceMuxMode.B
      ),
      // Shift left B operation
      Mult18Config(
        name = "SHIFT_LEFT_B",
        aluMode = AluMode.SHIFTB,
        sourceA_MUX = SourceMuxMode.A,
        sourceB_MUX = SourceMuxMode.B
      ),
      // Shift right B operation
      Mult18Config(
        name = "SHIFT_RIGHT_B",
        aluMode = AluMode.SHIFTB,
        sourceA_MUX = SourceMuxMode.A,
        sourceB_MUX = SourceMuxMode.SRIB
      ),
      // Shift left A and B operation
      Mult18Config(
        name = "SHIFT_LEFT_AB",
        aluMode = AluMode.SHIFTAB,
        sourceA_MUX = SourceMuxMode.A,
        sourceB_MUX = SourceMuxMode.B
      ),
      // Shift right A and B operation
      Mult18Config(
        name = "SHIFT_RIGHT_AB",
        aluMode = AluMode.SHIFTAB,
        sourceA_MUX = SourceMuxMode.SRIA,
        sourceB_MUX = SourceMuxMode.SRIB
      )
    ).padTo(
      64,
      Mult18Config()
    ) // Pad the sequence with default configs if there are less than 64 entries
  )

  // Select the config based on the input selection
  val config = configs.read(io.sel)
  val sysDSP = new SysDSP_Wrapper(config)

  // Connect inputs to the sysDSP wrapper
  sysDSP.io.A := io.a.asUInt
  sysDSP.io.B := io.b.asUInt
  sysDSP.io.C := io.c.asUInt
  sysDSP.io.cascadeIn := io.cascadeIn.asUInt

  // Connect outputs
  io.result := sysDSP.io.M.asSInt
  io.cascadeOut := sysDSP.io.cascadeOut.asSInt
}
