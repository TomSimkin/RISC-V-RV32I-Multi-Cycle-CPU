# 32-bit RISC-V Multi-Cycle CPU Design and Implementation

A complete, from-scratch implementation of a 32-bit RISC-V processor supporting the RV32I instruction set architecture. This project demonstrates advanced digital design principles through systematic component development, comprehensive testing, and professional verification practices.

## 🏆 Key Achievements
- ✅ Complete RV32I core instruction implementation (arithmetic, logic, control flow, memory)
- ✅ Multi-cycle architecture with advanced pipeline management  
- ✅ 100% test coverage across 6 comprehensive verification suites
- ✅ Professional-grade VHDL with hierarchical design methodology
- ✅ Systematic golden file verification framework
- ✅ Detailed performance analysis and timing characterization

## 🔧 Technical Specifications
- **Architecture**: 32-bit RISC-V Multi-cycle Processor
- **ISA Support**: RV32I (Core Integer Instructions)  
- **Performance**: 3.6 CPI average, 50-100 MHz target frequency
- **Verification**: 6 test suites with automated validation
- **Implementation**: Professional VHDL with complete documentation

## 📁 Repository Structure

**VHDL Source Files:**
- `alu.vhd` - Arithmetic Logic Unit implementation
- `branch_cmp.vhd` - Branch comparison logic
- `control_unit.vhd` - Main control unit with FSM
- `cpu_top.vhd` - Top-level CPU integration
- `cpu_top_tb.vhd` - Testbench for simulation
- `decoder.vhd` - Instruction decoder
- `pc_unit.vhd` - Program counter management
- `ram.vhd` - Memory subsystem
- `register_file.vhd` - Register file implementation

**Test Programs:**
- `arithmetic.mem` - Arithmetic test program
- `branch.mem` - Branch test program
- `jump.mem` - Jump test program
- `logic.mem` - Logic operations test program
- `memory.mem` - Memory access test program
- `shift.mem` - Shift operations test program

**Verification Files:**
- `arithmetic_golden.txt` - Expected results for arithmetic tests
- `branch_golden.txt` - Expected results for branch tests
- `jump_golden.txt` - Expected results for jump tests
- `logic_golden.txt` - Expected results for logic tests
- `memory_golden.txt` - Expected results for memory tests
- `shift_golden.txt` - Expected results for shift tests

**Documentation:**
- `CPU Explanation.pdf` - Complete project documentation

## 🚀 Getting Started
1. Clone the repository
2. Load project in ModelSim or compatible VHDL simulator
3. Run test suites to verify functionality (change the tests in `cpu_top_tb`)
4. Review documentation for detailed implementation analysis

## 📊 Test Coverage
- Arithmetic Operations: ✅ 100% Pass
- Branch Instructions: ✅ 100% Pass  
- Jump Instructions: ✅ 100% Pass
- Logic Operations: ✅ 100% Pass
- Memory Access: ✅ 100% Pass
- Shift Operations: ✅ 100% Pass

## 📖 Documentation
Complete project documentation available in `CPU Explanation.pdf` including:
- Architecture overview and design rationale
- Component-level implementation details  
- Comprehensive verification methodology
- Performance analysis and timing results
- Professional project report (100+ pages)

## 🛠 Technologies Used
- **VHDL** - Hardware description and implementation
- **ModelSim** - Simulation and verification platform
- **RISC-V ISA** - Target instruction set architecture
- **Multi-cycle Design** - Advanced processor architecture

## 📈 Project Impact
This project demonstrates professional-level CPU design capabilities and systematic engineering methodology suitable for:
- Digital design engineering positions
- CPU architecture roles
- FPGA development opportunities
- Graduate-level computer architecture studies
