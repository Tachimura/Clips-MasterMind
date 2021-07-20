# Clips-MasterMind
MasterMind solver implemented with Clips</br>
It is composed of various modules to split the code:
- Bell / Alan, are 2 modules of agents that can solve the problem (mainly Adam)
- Game /Main manage the flow and control the responses of every guess

Alan is based on an implementation of Swaszek.</br>
It can be easly upgraded by producing the S set right before the next guess (instead of creating all combinations at the beginning and then cut it time by time) or we can also just generate one new assignment (instead of everyone) since we will choose just one.
