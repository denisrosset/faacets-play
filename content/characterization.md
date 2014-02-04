### Bell inequalities and the characterization of non-locality

Quantum computers are known to provide a clear advantage to specific tasks over classical computers, for example through Shor’s algorithm [1] or Quantum cryptography [2], but it is certainly not true for all tasks. Then, it is important to identify in which case quantum computers are better than classical computers. In this context, quantifying the power of quantum correlations, which are involved in most examples where quantum leads to an advantage over classical, becomes extremely important.

The fact that some quantum states can lead to a certain kind of correlations was studied much before the era of quantum information. First presented as a paradox in the Gedanken experiment of Einstein, Podolsky and Rosen [3] and re-expressed by Bohm [4], it is known that two distant parties, Alice and Bob, become correlated by performing measurements on their part of a shared maximally entangled system. Later, in 1964 John Bell showed that such correlations cannot be reproduced by any model based on local random variables only, even with the help of unlimited amount of shared randomness. Namely, any correlations which can be explained by a classical model where Alice and Bob share some random variables verify Bell's inequalities.

Thus, a classical computer with only shared randomness cannot reproduce such correlations. Nevertheless if we add some extra resources to this classical model it will surely be possible. The question is which resources we should add and how much of each resource we should use. The amount of resource needed will give a measure of the non-locality of the correlations, and characterize the quantum computer’s advantage over its classical analogue. These extra resources don't necessary have a physical interpretation but can be seen as abstract tool to characterize the non-locality. Examples of resources that can be considered include instantaneous communication between Alice and Bob, post-selection (the detector inefficiency or detection loophole, c.f. [highlight on loopholes](Loopholes in Bell experiments.md)), and a mathematical device called a non-local box.

#### How much extra resource?

One can use either of two approach to answer to this problem. The first one is to find an explicit classical protocol which reproduces non-local correlations with shared random variables and some extra resources. This approach will lead to an upper bound on the amount of resources needed. For example, to reproduce the correlations of the maximally entangled state with classical shared randomness and one of the three resources above, we can explicitly construct a protocol [5]. We will need only one extra bit of communication [6], detectors with an inefficiency probability of 1/3 [7], or one non-local box [8].

Another approach is to check the existence of such protocol by extending some Bell inequalities to create new inequalities with resources. The new inequalities will characterize a set of correlations obtained by locally shared random variables and some specific amount of additional resources. If the quantum state doesn’t violate these Bell inequalities with resources, then a classical protocol with this amount of resource could exist. Reversely, quantum correlations violating this generalized set of inequalities, implies that the amount of resource is not sufficient.

<p align="center">
  <img src="/assets/images/contrib/characterization_ill.jpg" alt="Three sets of correlations (probability distributions): local, quantum, and causal. The extremal points of the causal polytope define the non-local boxes. The faces of the local polytope determine Bell inequalities. The distances Cnl and Cq from the local distributions give a bound on the classic and quantum communication complexity of the probability distribution."/><br/>
<span class="figure_caption">Three sets of correlations (probability distributions): local, quantum, and causal. The extremal points of the causal polytope define the non-local boxes. The faces of the local polytope determine Bell inequalities. The distances Cnl and Cq from the local distributions give a bound on the classic and quantum communication complexity of the probability distribution.</span>
</p>

Before building their protocol, Bacon and Toner used this method and built a complete set of Bell inequalities with one bit of communication for a fixed number of measurements [9]. They found that correlations produced by quantum theory satisfy these sets of inequalities, thus allowing for the existence of their future protocol.

Generalized sets of Bell inequalities can also be constructed with post-selection or non-local boxes as resources, see respectively [10] [11]. The advantage of this method is to give a straightforward path to follow when we haven’t got any intuition about how to build an explicit protocol. Moreover because Bell inequalities define the local set for a finite number of measurements, it stresses the importance to study Bell inequalities with more than 2 inputs (measurements), for each parties.

Bell inequalities are the cornerstone of the study of quantum correlation’s resource. The more we can characterize local correlations with Bell inequalities, the better we can evaluate the power of the quantum non-local correlation.

<p align="right">
Julien Degorre<br/>
<p/>
<p align="right">
<span class="author_address">
Centre for Quantum Technologies<br/>
National University of Singapore<br/>
3 Science Drive 2<br/>
Singapore 117543<br/>
<span/>
</p>

<p align="right">
published online on 4 Feb 2014
</p>

#### References

[1] Peter W. Shor, *Polynomial-Time Algorithms for Prime Factorization and Discrete Logarithms on a Quantum Computer*, [Proceedings of the 35th Annual Symposium on the Foundations of Computer Science, editor S. Goldwasser, pages 124–134, New York, (1994)](http://dx.doi.org/10.1137/S0097539795293172).

[2] C. H. Bennett and G. Brassard, *Public-Key Distribution and Coin Tossing*, [Proceedings of the IEEE International Conference on Computers, Systems, and Signal Processing, Bangalore, India, pages 175–179, New York, (1984)](http://dx.doi.org/10.1016/j.tcs.2011.08.039).

[3] A. Einstein, B. Podolsky, and N. Rosen, *Can quantum-mechanical description of physical reality be considered complete?*, [Phys. Rev. **47**, 777 (1935)](http://dx.doi.org/10.1103/PhysRev.47.777).

[4] D. Bohm and Y. Aharonov, *Discussion of Experimental Proof for the Paradox of Einstein, Rosen, and Podolsky*, [Phys. Rev. **108**, 1070 (1957)](http://dx.doi.org/10.1103/PhysRev.108.1070).

[5] J. Degorre, S. Laplante, and J. Roland, *Simulating quantum correlations as a distributed sampling problem*, [Phys. Rev. A **72**, 062314 (2005)](http://dx.doi.org/10.1103/PhysRevA.72.062314).

[6] B. F. Toner and D. Bacon, *Communication Cost of Simulating Bell Correlations*, [Phys. Rev. Lett. **91**, 187904 (2003)](http://dx.doi.org/10.1103/PhysRevLett.91.187904).

[7] Nicolas Gisin and Bernard Gisin, *A local hidden variable model of quantum correlation exploiting the detection loophole*, [Phys. Lett. A **260**, 323 (1999)](http://dx.doi.org/10.1016/S0375-9601\(99\)00519-8).

[8] N. J. Cerf, N. Gisin, S. Massar, and S. Popescu, *Simulating Maximal Quantum Entanglement without Communication*, [Phys. Rev. Lett. **94**, 220403 (2005)](http://dx.doi.org/10.1103/PhysRevLett.94.220403).

[9] D. Bacon and B. F. Toner, *Bell Inequalities with Auxiliary Communication*, [Physical Review Letters **90**, 157904 (2003)](http://dx.doi.org/10.1103/PhysRevLett.90.157904).

[10] Cyril Branciard, *Detection loophole in Bell experiments: How postselection modifies the requirements to observe nonlocality*, [Phys. Rev. A **83**, 032123 (2011)](http://dx.doi.org/10.1103/PhysRevA.83.032123).

[11] N. Brunner, N. Gisin, and V. Scarani, *Entanglement and non-locality are different resources*, [New Journal of Physics **7**, 88 (2005)](http://dx.doi.org/10.1088/1367-2630/7/1/088).
