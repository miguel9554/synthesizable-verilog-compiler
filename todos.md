# Immediate TODOs

* Add clock and reset types.
* Clock and reset check, that they don't affect any logic.
* Simulate.

# Arch/high level TODOs

## Remove loc info from nodes

Currently we have a `loc` field in each node, which is used for error
reporting. However, this is not ideal because the info should not be in the
DFG. We should store it in a secondary data structure.

## Types

We have clock, reset and integer. Probably need more, user defined types?

## DFG and Module consolidation

DFG and Module have signals, inputs and outputs repeated and not linked.  We
shouldn't repeat this info, and DFG members should be linked to syntax elements
(I think curently any name can be added)

## Submodule lookup duplicates pass 1 extraction

When resolving hierarchy instantiations, the submodule is looked up by name
from the same flat list of parsed modules (via ModuleLookup). This means
ir_builder (pass 1) extracts the module structure and then resolveModule finds
it again by name string. Ideally the unresolved hierarchy instantiation should
carry a direct reference to its UnresolvedModule rather than requiring a
name-based lookup at resolution time.

## Check clock and reset

Actually we check clock and reset don't affect the functional logic of the
flops they clock/reset. We should check this for *all* clocks and resets, and
also for combinational logic.

## Constant folding: skipped bit-width-dependent optimizations

The constant folding pass (`src/passes/constant_fold.cpp`) skips the following
optimizations because bit-width information is not yet available in the DFG:

* **Reduction ops on 1-bit signals**: `REDUCTION_AND(1-bit x) → x`,
  `REDUCTION_OR(1-bit x) → x`, `REDUCTION_XOR(1-bit x) → x`,
  `REDUCTION_NAND(1-bit x) → ~x`, `REDUCTION_NOR(1-bit x) → ~x`,
  `REDUCTION_XNOR(1-bit x) → ~x`.
* **Shift amount range validation**: `SHL`/`ASR` with shift amount ≥ bit-width
  should be flagged or clamped. Currently only constant-folds when both
  operands are constant.
* **Constant masking/sign-extension**: Folded constants are stored as raw
  `int64_t` without truncation to the signal's actual bit-width.

These should be revisited once the DFG carries proper type/width annotations.

## Hierarchical constant folding

First constant folding step resolves the folding individual modules, but
doesn't propagate constants across module boundaries. We need a 2nd constant
folding in which we propagate bottom down all the constants we found in the
first pass, propagating inside each module again.

## HW IR vs High level IR

Currently many ops (POWER, DIV, LOGICAL_NOT) are actually high-level. After
constant folding and normalization, we should arrive to a subset of HW IR ops.
For correctness, we should enforce this in types, using enums for each family
of ops. We could probably parametrize the DFG by the op enum type.

## More normalization + constant folding simplification

We have just 4 rules, are missing much more.
Many that are in constant folding (x+0, x\*1, etc) are actually *normalization*.
Should move them from constant folding, that pass should just be an eval of pure
constant nodes.
