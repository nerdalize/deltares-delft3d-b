!> \mainpage Delft3D-WAQ documentation
!> Use the first \"Modules\" item or tab to reach grouped tables of source code\n
!> The following groups are distinguished:
!> \li input processing
!> \li functional modules for input processing
!> \li advection diffusion solvers
!> \li functional modules for simulation
!> \li water quality processes
!>
!> Use the \"File List\" item or \"Files\" tab to get an alphabetic list of all source files
!<

!> \defgroup input input processing
!> <table>
!> <tr><th>source file</th><th>Group</th><th>Function</th></tr>
!> <tr><td>dlwq00.f</td><td></td><td>Main program of the DELWAQ input processing step</td></tr>
!> <tr><td>dlwq01.f</td><td>1</td><td>Reads the model identification and substances IDs</td></tr>
!> <tr><td>dlwq02.f</td><td>2</td><td>Reads integration method; monitoring areas/transects and timers</td></tr>
!> <tr><td>dlwq03.f</td><td>3</td><td>Reads grid layout; features and the computational volumes</td></tr>
!> <tr><td>dlwq04.f</td><td>4</td><td>Reads flow dimensions and pointers and all transport information</td></tr>
!> <tr><td>dlwq05.f</td><td>5</td><td>Reads all inputs associated with open boundaries</td></tr>
!> <tr><td>dlwq06.f</td><td>6</td><td>Reads all inputs associated with waste loads and withdrawals</td></tr>
!> <tr><td>dlwq07.f</td><td>7</td><td>Reads all model constants, variables and distributed variables/functions - old style</td></tr>
!> <tr><td>dlwq7a.f</td><td>7</td><td>Reads all model constants, variables and distributed variables/functions - new style</td></tr>
!> <tr><td>dlwq08.f</td><td>8</td><td>Reads initial conditions</td></tr>
!> <tr><td>dlwq09.f</td><td>9</td><td>Defines variables for output per available output file</td></tr>
!> <tr><td>dlwqs1.f</td><td>s</td><td>Defines process steering for statistical output processing</td></tr>
!> <tr><td>dlwqp1.f</td><td>p</td><td>Defines process steering for all water quality processing</td></tr>
!> <tr><td>space.f</td><td></td><td>Computes and reports array space sizes and sets memory pointers at run time</td></tr>
!> </table>
!> \defgroup inp_funcs functional modules for input processing
!> <table>
!> <tr><th>source file</th><th>Role</th></tr>
!> <tr><td>opt0.f  </td><td>Top routine of system to read old style matrix input</td></tr>
!> <tr><td>dlwq5a.f</td><td>New style intuitive flexible input processing for boundaries and waste loads</td></tr>
!> <tr><td>dlwq7a.f</td><td>New style intuitive flexible input processing for constants and functions</td></tr>
!> </table>
!> \defgroup solvers advection diffusion solvers
!> <table>
!> <tr><th align=center>method</th><th>solver</th><th>Numerical method</th><th align=center>stability</th></tr>
!> <tr><td align=center>    </td><td>delwaq2.f</td><td>Main program of the simulations step</td><td></td></tr>
!> <tr><td align=center>   1 </td><td>dlwqn1.f</td><td>First order upwind in space and time</td><td align=center>explicit     </td></tr>
!> <tr><td></td><td></td><td><table>
!> <tr><td>dlwq16.f</td><td>Fills derivative with advection diffusion processes</td></tr>
!> <tr><td>dlwq18.f</td><td>Sets an explicit time step from the derivative</td></tr>
!> </table></td><td></td></tr>
!> <tr><td align=center>   2 </td><td>dlwqn2.f</td><td>First order upwind in space 2nd order Runge Kutta in time</td><td align=center>explicit     </td></tr>
!> <tr><td></td><td></td><td><table>
!> <tr><td>dlwq16.f</td><td>Fills derivative with advection diffusion processes</td></tr>
!> <tr><td>dlwq20.f</td><td>Sets a preliminary step of dt to provide concentrations</td></tr>
!> <tr><td>dlwq16.f</td><td>Fills derivative with advection diffusion processes from these new concentrations</td></tr>
!> <tr><td>dlwq18.f</td><td>Sets a full final step of 2*dt</td></tr>
!> </table></td><td></td></tr>
!> <tr><td align=center>   3 </td><td>dlwqn3.f</td><td>Second order Lax Wendrof in space and time</td><td align=center>explicit     </td></tr>
!> <tr><td></td><td></td><td><table>
!> <tr><td>dlwq30.f</td><td>Fills derivative with advection diffusion processes</td></tr>
!> <tr><td>dlwq18.f</td><td>Sets an explicit time step from the derivative</td></tr>
!> </table></td><td></td></tr>
!> <tr><td align=center>   4 </td><td>dlwqn4.f</td><td>Horizontal ADI on regular grid, vertical implicit central</td><td align=center>half implicit</td></tr>
!> <tr><td></td><td></td><td><table>
!> <tr><td>dlwq40.f</td><td>Fills derivative with advection diffusion processes in one horizontal explicit direction</td></tr>
!> <tr><td>dlwq42.f</td><td>Sets an explicit time step from the derivative</td></tr>
!> <tr><td>dlwq43.f</td><td>Fills derivative with advection diffusion processes in other horizontal direction, performs implicit double sweep</td></tr>
!> <tr><td>dlwq44.f</td><td>Update the necessary arrays</td></tr>
!> <tr><td>dlwq46.f</td><td>Mass balances for this method</td></tr>
!> </table></td><td></td></tr>
!> <tr><td align=center>   5 </td><td>dlwqn5.f</td><td>Flux corrected transport according to Boris and Book (FCT)</td><td align=center>explicit     </td></tr>
!> <tr><td></td><td></td><td><table>
!> <tr><td>dlwq50.f</td><td>Fills derivative with advection processes only</td></tr>
!> <tr><td>dlwq18.f</td><td>Sets an explicit time step from the derivative</td></tr>
!> <tr><td>dlwq51.f</td><td>Perform a flux correction step on this preliminary result</td></tr>
!> <tr><td>dlwq52.f</td><td>Update the necessary arrays</td></tr>
!> </table></td><td></td></tr>
!> <tr><td align=center>   6 </td><td>dlwqn6.f</td><td>Stationary first order upwind in space, direct method</td><td align=center>implicit     </td></tr>
!> <tr><td></td><td></td><td><table>
!> <tr><td>dlwq60.f</td><td>Scales derivative coming from processes routine</td></tr>
!> <tr><td>dlwq61.f</td><td>Fills the diagonal for 1 substance</td></tr>
!> <tr><td>dlwq62.f</td><td>Fills the sparce matrix with advection diffusion terms</td></tr>
!> <tr><td>dlwq67.f</td><td>Sets diagonal entries of approximately 0.0 at 1.0</td></tr>
!> <tr><td>delmat.f</td><td>Direct matrix solver</td></tr>
!> <tr><td>dlwq63.f</td><td>Update the necessary arrays</td></tr>
!> <tr><td>dlwq64.f</td><td>Makes mass balances for this method</td></tr>
!> <tr><td>dlwq66.f</td><td>Update the necessary arrays</td></tr>
!> </table></td><td></td></tr>
!> <tr><td align=center>   7 </td><td>dlwqn7.f</td><td>Stationary central in space, direct method</td><td align=center>implicit     </td></tr>
!> <tr><td></td><td></td><td><table>
!> <tr><td>dlwq60.f</td><td>Scales derivative coming from processes routine</td></tr>
!> <tr><td>dlwq61.f</td><td>Fills the diagonal for 1 substance</td></tr>
!> <tr><td>dlwq70.f</td><td>Fills the sparce matrix with advection diffusion terms</td></tr>
!> <tr><td>dlwq67.f</td><td>Sets diagonal entries of approximately 0.0 at 1.0</td></tr>
!> <tr><td>delmat.f</td><td>Direct matrix solver</td></tr>
!> <tr><td>dlwq63.f</td><td>Update the necessary arrays</td></tr>
!> <tr><td>dlwq71.f</td><td>Makes mass balances for this method</td></tr>
!> <tr><td>dlwq66.f</td><td>Update the necessary arrays</td></tr>
!> </table></td><td></td></tr>
!> <tr><td align=center>   8 </td><td>dlwqn8.f</td><td>Stationary first order upwind in space, iterative method</td><td align=center>obsolete     </td></tr>
!> <tr><td></td><td></td><td><h2>This method is obsolete</h2></td><td></td></tr>
!> <tr><td align=center>   9 </td><td>dlwqn9.f</td><td>Stationary central in space, iterative method</td><td align=center>obsolete     </td></tr>
!> <tr><td></td><td></td><td><h2>This method is obsolete</h2></td><td></td></tr>
!> <tr><td align=center>  10 </td><td>dlwqnb.f</td><td>First order upwind in space, implicit, direct method</td><td align=center>implicit     </td></tr>
!> <tr><td></td><td></td><td><table>
!> <tr><td>dlwqb8.f</td><td>Restores CONC array from MASS array</td></tr>
!> <tr><td>dlwqb3.f</td><td>New volumes from optional computational volumes</td></tr>
!> <tr><td>dlwqb1.f</td><td>Fills the diagonal for 1 substance</td></tr>
!> <tr><td>dlwqb6.f</td><td>Computes right hand side</td></tr>
!> <tr><td>dlwqb7.f</td><td>Fills the sparce matrix with advection diffusion terms</td></tr>
!> <tr><td>dlwqb9.f</td><td>Fills the sparce matrix with advection diffusion terms</td></tr>
!> <tr><td>delmat.f</td><td>Direct matrix solver</td></tr>
!> <tr><td>dlwqb2.f</td><td>Update the necessary arrays</td></tr>
!> <tr><td>dlwqb5.f</td><td>Makes mass balances for this method</td></tr>
!> <tr><td>dlwqb4.f</td><td>Update the necessary arrays</td></tr>
!> </table></td><td></td></tr>
!> <tr><td align=center>  11 </td><td>dlwqnc.f</td><td>First order upwind in space, implicit central vertical</td><td align=center>explicit     </td></tr>
!> <tr><td></td><td></td><td><table>
!> <tr><td>dlwq16.f</td><td>Fills derivative with advection diffusion processes</td></tr>
!> <tr><td>dlwq42.f</td><td>Sets an explicit time step from the derivative</td></tr>
!> <tr><td>dlwqd1.f</td><td>Sets an implicit time step for the central transport of the vertical</td></tr>
!> <tr><td>dlwqd2.f</td><td>Forester filter to avoid spurious extremes</td></tr>
!> <tr><td>dlwq44.f</td><td>Update the necessary arrays</td></tr>
!> <tr><td>dlwq46.f</td><td>Mass balances for this method</td></tr>
!> </table></td><td></td></tr>
!> <tr><td align=center>  12 </td><td>dlwqnd.f</td><td>FCT horizontal, central implicit vertical</td><td align=center>explicit     </td></tr>
!> <tr><td></td><td></td><td><table>
!> <tr><td>dlwq50.f</td><td>Fills derivative with advection processes only</td></tr>
!> <tr><td>dlwq18.f</td><td>Sets an explicit time step from the derivative</td></tr>
!> <tr><td>dlwq51.f</td><td>Perform a flux correction step on this preliminary result</td></tr>
!> <tr><td>dlwq52.f</td><td>Update the necessary arrays</td></tr>
!> <tr><td>dlwq42.f</td><td>Sets volumes on the diagonal</td></tr>
!> <tr><td>dlwqd1.f</td><td>Sets an implicit time step for the central transport of the vertical</td></tr>
!> <tr><td>dlwqd2.f</td><td>Forester filter to avoid spurious extremes</td></tr>
!> <tr><td>dlwq44.f</td><td>Update the necessary arrays</td></tr>
!> <tr><td>dlwq46.f</td><td>Mass balances for this method</td></tr>
!> </table></td><td></td></tr>
!> <tr><td align=center>  13 </td><td>dlwqne.f</td><td>First order upwind in space, upwind implicit vertical</td><td align=center>explicit     </td></tr>
!> <tr><td></td><td></td><td><table>
!> <tr><td>dlwq16.f</td><td>Fills derivative with advection diffusion processes</td></tr>
!> <tr><td>dlwq42.f</td><td>Sets volumes on the diagonal</td></tr>
!> <tr><td>dlwqe1.f</td><td>Sets an implicit time step for the upwind transport of the vertical</td></tr>
!> <tr><td>dlwq44.f</td><td>Update the necessary arrays</td></tr>
!> <tr><td>dlwq46.f</td><td>Mass balances for this method</td></tr>
!> </table></td><td></td></tr>
!> <tr><td align=center>  14 </td><td>dlwqna.f</td><td>FCT horizontal, upwind implicit vertical</td><td align=center>explicit     </td></tr>
!> <tr><td></td><td></td><td><table>
!> <tr><td>dlwq50.f</td><td>Fills derivative with advection processes only</td></tr>
!> <tr><td>dlwq18.f</td><td>Sets an explicit time step from the derivative</td></tr>
!> <tr><td>dlwq51.f</td><td>Perform a flux correction step on this preliminary result</td></tr>
!> <tr><td>dlwq52.f</td><td>Update the necessary arrays</td></tr>
!> <tr><td>dlwq42.f</td><td>Sets volumes on the diagonal</td></tr>
!> <tr><td>dlwqe1.f</td><td>Sets an implicit time step for the upwind transport of the vertical</td></tr>
!> <tr><td>dlwq44.f</td><td>Update the necessary arrays</td></tr>
!> <tr><td>dlwq46.f</td><td>Mass balances for this method</td></tr>
!> </table></td><td></td></tr>
!> <tr><td align=center>  15 </td><td>dlwqnf.f</td><td>First order upwind, iterative GMRES solver</td><td align=center>implicit     </td></tr>
!> <tr><td></td><td></td><td><table>
!> <tr><td>dlwqf1.f</td><td>Initializes the necessary pointers for matrices and vectors for fast solver</td></tr>
!> <tr><td>dlwqb8.f</td><td>Restores CONC array from MASS array</td></tr>
!> <tr><td>dlwqm7.f</td><td>Makes mixing length array according to numerical settings</td></tr>
!> <tr><td>dlwqf2.f90</td><td>Initializes the diagonal</td></tr>
!> <tr><td>dlwqf3.f90</td><td>Fills the sparce matrix with advection and diffusion terms (one substance at a time)</td></tr>
!> <tr><td>dlwqf4.f90</td><td>Computes Right Hand Side (one substance at a time)</td></tr>
!> <tr><td>sgmres.f90</td><td>The Krilov subspace iterative sparce matrix solver</td></tr>
!> <tr><td>dlwqf6.f90</td><td>Copies the solution for this substance in the total substances array</td></tr>
!> <tr><td>dlwqb5.f</td><td>Makes mass balances for this method</td></tr>
!> <tr><td>dlwqb4.f</td><td>Update the necessary arrays</td></tr>
!> </table></td><td></td></tr>
!> <tr><td align=center>  16 </td><td>dlwqng.f</td><td>Upwind horizontally, central vertically, GMRES solver</td><td align=center>implicit     </td></tr>
!> <tr><td></td><td></td><td><table>
!> <tr><td>dlwqf1.f</td><td>Initializes the necessary pointers for matrices and vectors for fast solver</td></tr>
!> <tr><td>dlwqb8.f</td><td>Restores CONC array from MASS array</td></tr>
!> <tr><td>dlwqf2.f90</td><td>Initializes the diagonal</td></tr>
!> <tr><td>dlwqg3.f90</td><td>Fills the sparce matrix with advection and diffusion terms (one substance at a time)</td></tr>
!> <tr><td>dlwqf4.f90</td><td>Computes Right Hand Side (one substance at a time)</td></tr>
!> <tr><td>sgmres.f90</td><td>The Krilov subspace iterative sparce matrix solver</td></tr>
!> <tr><td>dlwqf6.f90</td><td>Copies the solution for this substance in the total substances array</td></tr>
!> <tr><td>dlwqb5.f</td><td>Makes mass balances for this method</td></tr>
!> <tr><td>dlwqb4.f</td><td>Update the necessary arrays</td></tr>
!> </table></td><td></td></tr>
!> <tr><td align=center>  17 </td><td>dlwqnh.f</td><td>First order upwind, iterative stationary GMRES solver            </td><td align=center>implicit     </td></tr>
!> <tr><td></td><td></td><td><table>
!> <tr><td>dlwqm7.f</td><td>Makes mixing length array according to numerical settings</td></tr>
!> <tr><td>dlwqh1.f</td><td>Initializes the diagonal</td></tr>
!> <tr><td>dlwqf3.f90</td><td>Fills the sparce matrix with advection and diffusion terms (one substance at a time)</td></tr>
!> <tr><td>dlwqh3.f</td><td>Computes Right Hand Side (one substance at a time)</td></tr>
!> <tr><td>sgmres.f90</td><td>The Krilov subspace iterative sparce matrix solver</td></tr>
!> <tr><td>dlwqf6.f90</td><td>Copies the solution for this substance in the total substances array</td></tr>
!> <tr><td>dlwq64.f</td><td>Makes mass balances for this method</td></tr>
!> <tr><td>dlwq66.f</td><td>Update the necessary arrays</td></tr>
!> </table></td><td></td></tr>
!> <tr><td align=center>  18 </td><td>dlwqni.f</td><td>Upwind horizontally, central vertically, stationary GMRES solver </td><td align=center>implicit     </td></tr>
!> <tr><td></td><td></td><td><table>
!> <tr><td>dlwqm7.f</td><td>Makes mixing length array according to numerical settings</td></tr>
!> <tr><td>dlwqh1.f</td><td>Initializes the diagonal</td></tr>
!> <tr><td>dlwqf3.f90</td><td>Fills the sparce matrix with advection and diffusion terms (one substance at a time)</td></tr>
!> <tr><td>dlwqh3.f</td><td>Computes Right Hand Side (one substance at a time)</td></tr>
!> <tr><td>sgmres.f90</td><td>The Krilov subspace iterative sparce matrix solver</td></tr>
!> <tr><td>dlwqf6.f90</td><td>Copies the solution for this substance in the total substances array</td></tr>
!> <tr><td>dlwq64.f</td><td>Makes mass balances for this method</td></tr>
!> <tr><td>dlwq66.f</td><td>Update the necessary arrays</td></tr>
!> </table></td><td></td></tr>
!> <tr><td align=center>  19 </td><td>dlwqnj.f</td><td>ADI solver of Delft3D-FLOW (difu) 3rd order, upwind vertically   </td><td align=center>half implicit</td></tr>
!> <tr><td></td><td></td><td><table>
!> <tr><td>dlinit.f</td><td>Initializes the Delft3D-Flow ADE solver arrays</td></tr>
!> <tr><td>dlconv.f</td><td>Converts the hydrodynamics to Delft3D-Flow ADE full matrix arrays</td></tr>
!> <tr><td>dldifu.f</td><td>Aims to be identical to the Delft3D-Flow difu.f90 source file. Is difficult to maintain.</td></tr>
!> <tr><td>dlflux.f</td><td>Computes fluxes for balances for this solver</td></tr>
!> <tr><td>dlmasb.f</td><td>Makes the mass balances from these fluxes</td></tr>
!> <tr><td>dlback.f</td><td>Back conversion of some arrays</td></tr>
!> </table></td><td></td></tr>
!> <tr><td align=center>  20 </td><td>dlwqnj.f</td><td>ADI solver of Delft3D-FLOW (difu) 3rd order, central vertically  </td><td align=center>half implicit</td></tr>
!> <tr><td></td><td></td><td><table>
!> <tr><td>dlinit.f</td><td>Initializes the Delft3D-Flow ADE solver arrays</td></tr>
!> <tr><td>dlconv.f</td><td>Converts the hydrodynamics to Delft3D-Flow ADE full matrix arrays</td></tr>
!> <tr><td>dldifu.f</td><td>Aims to be identical to the Delft3D-Flow difu.f90 source file. Is difficult to maintain.</td></tr>
!> <tr><td>dlflux.f</td><td>Computes fluxes for balances for this solver</td></tr>
!> <tr><td>dlmasb.f</td><td>Makes the mass balances from these fluxes</td></tr>
!> <tr><td>dlback.f</td><td>Back conversion of some arrays</td></tr>
!> <tr><td>dlwqd2.f</td><td>Forester filter to avoid spurious extremes</td></tr>
!> </table></td><td></td></tr>
!> <tr><td align=center>  21 </td><td>dlwqnm.f</td><td>Self adjusting theta method, Salezac limiter                     </td><td align=center>implicit     </td></tr>
!> <tr><td></td><td></td><td><table>
!> <tr><td>dlwqf1.f</td><td>Initializes the necessary pointers for matrices and vectors for fast solver</td></tr>
!> <tr><td>dlwqm7.f</td><td>Makes mixing length array according to numerical settings</td></tr>
!> <tr><td>dlwqm0.f</td><td>Makes specific flow and dispersion arrays for this substance</td></tr>
!> <tr><td>dlwqm1.f</td><td>Computes space and time varyin theta coefficients</td></tr>
!> <tr><td>dlwq_output_theta.f</td><td>Saves a theta field for output for the first substance only (often continuity)</td></tr>
!> <tr><td>dlwqm2.f</td><td>Constructs the sparse advection diffusion matrix</td></tr>
!> <tr><td>dlwqm3.f</td><td>Constructs the right hand side</td></tr>
!> <tr><td>sgmres.f90</td><td>The Krilov subspace iterative sparce matrix solver</td></tr>
!> <tr><td>dlwqm4.f</td><td>Makes the mass balance</td></tr>
!> <tr><td>dlwqm5.f</td><td>Flux correction according to Salezac</td></tr>
!> <tr><td>dlwqb4.f</td><td>Update the necessary arrays</td></tr>
!> <tr><td>proint.f</td><td>Integrate the fluxes at dump segments</td></tr>
!> </table></td><td></td></tr>
!> <tr><td align=center>  22 </td><td>dlwqnm.f</td><td>Self adjusting theta method, Boris and Book limiter              </td><td align=center>implicit     </td></tr>
!> <tr><td></td><td></td><td><table>
!> <tr><td>dlwqf1.f</td><td>Initializes the necessary pointers for matrices and vectors for fast solver</td></tr>
!> <tr><td>dlwqm7.f</td><td>Makes mixing length array according to numerical settings</td></tr>
!> <tr><td>dlwqm0.f</td><td>Makes specific flow and dispersion arrays for this substance</td></tr>
!> <tr><td>dlwqm1.f</td><td>Computes space and time varyin theta coefficients</td></tr>
!> <tr><td>dlwq_output_theta.f</td><td>Saves a theta field for output for the first substance only (often continuity)</td></tr>
!> <tr><td>dlwqm2.f</td><td>Constructs the sparse advection diffusion matrix</td></tr>
!> <tr><td>dlwqm3.f</td><td>Constructs the right hand side</td></tr>
!> <tr><td>sgmres.f90</td><td>The Krilov subspace iterative sparce matrix solver</td></tr>
!> <tr><td>dlwqm4.f</td><td>Makes the mass balance</td></tr>
!> <tr><td>dlwqm8.f</td><td>Flux correction according to Boris and Book</td></tr>
!> <tr><td>dlwqb4.f</td><td>Update the necessary arrays</td></tr>
!> <tr><td>proint.f</td><td>Integrate the fluxes at dump segments</td></tr>
!> </table></td><td></td></tr>
!> </table>
!> \defgroup run_funcs functional modules for simulation
!> <table>
!> <tr><th>source file</th><th>Role</th></tr>
!> <tr><td>dlwqi0.f</td><td>Initialises the system </td></tr>
!> <tr><td>proces.f</td><td>Deals with all water quality processes </td></tr>
!> <tr><td>dlwqo2.f</td><td>Deals with all output of model results </td></tr>
!> <tr><td>dlwq17.f</td><td>Deals with the resolving of open boundaries (dlwq_boundio should be included in here) </td></tr>
!> <tr><td>dlwq15.f</td><td>Deals with the resolving of all waste loads </td></tr>
!> <tr><td>dlwqt0.f</td><td>Deals with the resolving of all other external forcing </td></tr>
!> <tr><td>dlwqce.f</td><td>Makes closure error corrections </td></tr>
!> <tr><td>dlwq13.f</td><td>Writes the restart file </td></tr>
!> </table>
!> \defgroup waterquality water quality processes
!> <table>
!> <tr><th>group</th><th>source file</th><th>function</th><th>remark</th></tr>
!> <tr><td>age residense time</td><td></td><td></td><td></td></tr>
!> <tr><td></td><td>restim.f</td><td>Residence time per volume, for advective transport only</td><td></td></tr>
!> <tr><td></td><td>weirch.f</td><td>Calculates the residence time of a volume</td><td></td></tr>
!> <tr><td></td><td>watage.f</td><td>Age of water through the tracer substances</td><td></td></tr>
!> <tr><td>statistics</td><td></td><td></td><td></td></tr>
!> <tr><td></td><td>staday.f</td><td>Periodic (day) average of a given substance</td><td></td></tr>
!> <tr><td></td><td>stadpt.f</td><td>Depth-averaged, max and min value per timestep</td><td></td></tr>
!> <tr><td></td><td>stadsc.f</td><td>Mean, min, max, stdev of an output variable</td><td></td></tr>
!> <tr><td></td><td>stageo.f</td><td>Geometric mean of a variable during a certian time span</td><td></td></tr>
!> <tr><td></td><td>staprc.f</td><td>Exceedence frequency, its complement and the mean</td><td></td></tr>
!> <tr><td></td><td>staqtl.f</td><td>Quantiles for a given substance during a given period</td><td></td></tr>
!> <tr><td>bacteria</td><td></td><td></td><td></td></tr>
!> <tr><td></td><td>bacmrt.f</td><td>Mortality of bacteria depending on UV-light, salinity and temperature</td><td></td></tr>
!> <tr><td></td><td>groab.f</td><td>Aerobic growth of nitrifying autotrophic bact.</td><td></td></tr>
!> <tr><td></td><td>grohb.f</td><td>Growth of heterotrophic bact. on Cdom and Cfom</td><td></td></tr>
!> <tr><td></td><td>lysis.f</td><td>Lysis of bacteria</td><td></td></tr>
!> <tr><td></td><td>sedcar.f</td><td>Sedimentation routine used for IMx, OOC, algae, BOD pools, bacteria etc.</td><td></td></tr>
!> <tr><td></td><td>sumcol.f</td><td>Sum of all Coliform Bacteria</td><td></td></tr>
!> <tr><td>inorganic sediment</td><td></td><td></td><td></td></tr>
!> <tr><td></td><td>atmdep.f</td><td>Atmosferic deposition and diffuse input of IMx, N, P, Org_us and Metals</td><td></td></tr>
!> <tr><td></td><td>burcar.f</td><td>Burial of IMx, DetC, OOC, Green, Diat and AAP</td><td></td></tr>
!> <tr><td></td><td>calsnd.f</td><td>Transport of non-cohesive sediment</td><td></td></tr>
!> <tr><td></td><td>calsed.f</td><td>Sedimentation velocity IMx, DetC OOC, BODC, all algea = f (Temp SS Sal)</td><td></td></tr>
!> <tr><td></td><td>digcar.f</td><td>Digging of IMx, DetC, OOC, Green, Diat and AAP</td><td></td></tr>
!> <tr><td></td><td>dmvol.f</td><td>Volume of dry matter in a segment</td><td></td></tr>
!> <tr><td></td><td>rescar.f</td><td>Resuspension of particulates by fraction * resdm, from S2 if S1 is exhausted</td><td></td></tr>
!> <tr><td></td><td>sedcar.f</td><td>Sedimentation routine used for IMx, OOC, algae, BOD pools, bacteria etc.</td><td></td></tr>
!> <tr><td></td><td>wkcomp.f</td><td>Computes sum parameters from fractions (GEM)</td><td></td></tr>
!> <tr><td>BOD oxygen OC</td><td></td><td></td><td></td></tr>
!> <tr><td></td><td>bod.f</td><td>Calculates the bod5 and bod-inf from available model substances</td><td></td></tr>
!> <tr><td></td><td>bodcod.f</td><td>Decay of BOD, COD and NBOD and associated oxygen consumption</td><td></td></tr>
!> <tr><td></td><td>botmin.f</td><td>Mineralisation of organic substances and desorption of AAP in the bed (S1,S2) for C, N, P and Si.</td><td></td></tr>
!> <tr><td></td><td>burcar.f</td><td>Burial of IMx, DetC, OOC, Green, Diat and AAP</td><td></td></tr>
!> <tr><td></td><td>calsed.f</td><td>Sedimentation velocity IMx, DetC OOC, BODC, all algea = f (Temp SS Sal)</td><td></td></tr>
!> <tr><td></td><td>d40blo.f</td><td>BLOOM II algae module</td><td></td></tr>
!> <tr><td></td><td>d40swi.f</td><td>Sediment-water interaction, the SWITCH module</td><td></td></tr>
!> <tr><td></td><td>decbod.f</td><td>Oxydation of BOD-fractions with Monod kinetics for the TEWOR models</td><td></td></tr>
!> <tr><td></td><td>decfsn.f</td><td>Mineralisation of fast composing detritus and conversion of the C:N and C:P ratios</td><td>almost identical to decfst</td></tr>
!> <tr><td></td><td>decfst.f</td><td>Mineralisation of fast composing detritus with equal rations for C, N and P</td><td>why 2 separate routines ?</td></tr>
!> <tr><td></td><td>decref.f</td><td>Mineralisation of refractory detritus C, N and P</td><td>why 2 separate routines ?</td></tr>
!> <tr><td></td><td>decren.f</td><td>Mineralisation of refractory detritus C, N and P</td><td>is identical to decref !!</td></tr>
!> <tr><td></td><td>decsln.f</td><td>Mineralisation of fast composing detritus and conversion of the C:N and C:P ratios</td><td>almost identical to decslw</td></tr>
!> <tr><td></td><td>decslw.f</td><td>Mineralisation of fast composing detritus with equal rations for C, N and P</td><td>why 2 separate routines ?</td></tr>
!> <tr><td></td><td>densed.f</td><td>Denitrification in sediment</td><td></td></tr>
!> <tr><td></td><td>denwat.f</td><td>Denitrification in water column</td><td></td></tr>
!> <tr><td></td><td>digcar.f</td><td>Digging of IMx, DetC, OOC, Green, Diat and AAP</td><td></td></tr>
!> <tr><td></td><td>gemmfb.f</td><td>GEM microfytobenthos production</td><td></td></tr>
!> <tr><td></td><td>gemmnd.f</td><td>GEM algae primary production module</td><td></td></tr>
!> <tr><td></td><td>gemmin.f</td><td>GEM mineralisation</td><td></td></tr>
!> <tr><td></td><td>groab.f</td><td>Aerobic growth of nitrifying autotrophic bact.</td><td></td></tr>
!> <tr><td></td><td>makpoc.f</td><td>Derive OOC from IM-fractions and percentage POM in IMx</td><td></td></tr>
!> <tr><td></td><td>methox.f</td><td>Oxidation of methane (new and generic)</td><td></td></tr>
!> <tr><td></td><td>minlim.f</td><td>Mineralisation organic C, N, P and Si</td><td></td></tr>
!> <tr><td></td><td>nitrif.f</td><td>Nitrification of ammonium + decay of CBOD</td><td></td></tr>
!> <tr><td></td><td>oxymin.f</td><td>Potential daily mimimum dissolved oxygen concentration</td><td></td></tr>
!> <tr><td></td><td>posoxy.f</td><td>Returns positive oxygen concentration or zero</td><td></td></tr>
!> <tr><td></td><td>pripro.f</td><td>Nett primary production and mortality DYNAMO algae</td><td></td></tr>
!> <tr><td></td><td>ptewor.f</td><td>Production fluxes for TEWOR+</td><td></td></tr>
!> <tr><td></td><td>rear.f</td><td>Reaeration of carbon dioxide and oxygen</td><td></td></tr>
!> <tr><td></td><td>rescar.f</td><td>Resuspension of particulates by fraction * resdm, from S2 if S1 is exhausted</td><td></td></tr>
!> <tr><td></td><td>sdppro.f</td><td>Traditional algal growth module (DYNAMO)</td><td></td></tr>
!> <tr><td></td><td>satco2.f</td><td>Saturation concentration carbon dioxide</td><td></td></tr>
!> <tr><td></td><td>satoxy.f</td><td>Saturation concentration of oxygen</td><td></td></tr>
!> <tr><td></td><td>sedcar.f</td><td>Sedimentation routine used for IMx, OOC, algae, BOD pools, bacteria etc.</td><td></td></tr>
!> <tr><td></td><td>sednut.f</td><td>Sedimentation of nutrients in the organic carbon matrix (GEM)</td><td></td></tr>
!> <tr><td></td><td>sedox.f</td><td>Sediment oxygen demand</td><td></td></tr>
!> <tr><td></td><td>sedsod.f</td><td>Sedimentation of oxygen demand</td><td></td></tr>
!> <tr><td></td><td>strear.f</td><td>Aeration at weirs (Gameson and Nakasone) (input is array of structures)</td><td></td></tr>
!> <tr><td></td><td>swbur.f</td><td>Fluxes in the water bed due to burial and digging</td><td></td></tr>
!> <tr><td></td><td>varoxy.f</td><td>Variation of oxygen due to variation in primary production within day</td><td></td></tr>
!> <tr><td></td><td>weirox.f</td><td>Rearation at weirs (input is a parameter indicating weir or not)</td><td></td></tr>
!> <tr><td></td><td>wkcomp.f</td><td>Composition</td><td></td></tr>
!> <tr><td>nutrients</td><td></td><td></td><td></td></tr>
!> <tr><td></td><td>adspo4.f</td><td>P-ad/desorption to particulate inorganic matter. 3 options for sorption formulation.</td><td></td></tr>
!> <tr><td></td><td>atmdep.f</td><td>Atmosferic deposition and diffuse input of IMx, N, P, Org_us and Metals</td><td></td></tr>
!> <tr><td></td><td>botmin.f</td><td>Mineralisation of organic substances and desorption of AAP in the bed (S1,S2) for C, N, P and Si.</td><td></td></tr>
!> <tr><td></td><td>burcar.f</td><td>Burial of IMx, DetC, OOC, Green, Diat and AAP</td><td></td></tr>
!> <tr><td></td><td>burnut.f</td><td>Burial of nutrients in organicC from S2</td><td></td></tr>
!> <tr><td></td><td>consbl.f</td><td>Grazing module</td><td></td></tr>
!> <tr><td></td><td>d40blo.f</td><td>BLOOM II algae module</td><td></td></tr>
!> <tr><td></td><td>d40swi.f</td><td>Sediment-water interaction, the SWITCH module</td><td></td></tr>
!> <tr><td></td><td>decfsn.f</td><td>Mineralisation of fast composing detritus and conversion of the C:N and C:P ratios</td><td>almost identical to decfst</td></tr>
!> <tr><td></td><td>decfst.f</td><td>Mineralisation of fast composing detritus with equal rations for C, N and P</td><td>why 2 separate routines ?</td></tr>
!> <tr><td></td><td>decref.f</td><td>Mineralisation of refractory detritus C, N and P</td><td>why 2 separate routines ?</td></tr>
!> <tr><td></td><td>decren.f</td><td>Mineralisation of refractory detritus C, N and P</td><td>is identical to decref !!</td></tr>
!> <tr><td></td><td>decsln.f</td><td>Mineralisation of fast composing detritus and conversion of the C:N and C:P ratios</td><td>almost identical to decslw</td></tr>
!> <tr><td></td><td>decslw.f</td><td>Mineralisation of fast composing detritus with equal rations for C, N and P</td><td>why 2 separate routines ?</td></tr>
!> <tr><td></td><td>densed.f</td><td>Denitrification in sediment</td><td></td></tr>
!> <tr><td></td><td>denwat.f</td><td>Denitrification in water column</td><td></td></tr>
!> <tr><td></td><td>digcar.f</td><td>Digging of IMx, DetC, OOC, Green, Diat and AAP</td><td></td></tr>
!> <tr><td></td><td>dignut.f</td><td>Digging of nutrients in organicC</td><td></td></tr>
!> <tr><td></td><td>dissi.f</td><td>Dissolution of Si in opal (SWITCH defaults)</td><td></td></tr>
!> <tr><td></td><td>dmvol.f</td><td>Volume of dry matter in a segment</td><td></td></tr>
!> <tr><td></td><td>explfl.f</td><td>Simulation of explosive nutrient sediment release</td><td></td></tr>
!> <tr><td></td><td>ferdom.f</td><td>Fermentation of dissolved fermentable org. matter</td><td></td></tr>
!> <tr><td></td><td>gemcmp.f</td><td>GEM computation of C, N and P from stoichiometry</td><td></td></tr>
!> <tr><td></td><td>gemmfb.f</td><td>GEM microfytobenthos production</td><td></td></tr>
!> <tr><td></td><td>gemmin.f</td><td>GEM mineralisation</td><td></td></tr>
!> <tr><td></td><td>gemmnd.f</td><td>GEM algae primary production module</td><td></td></tr>
!> <tr><td></td><td>groab.f</td><td>Aerobic growth of nitrifying autotrophic bact.</td><td></td></tr>
!> <tr><td></td><td>grohb.f</td><td>Growth of heterotrophic bact. on Cdom and Cfom</td><td></td></tr>
!> <tr><td></td><td>hydpom.f</td><td>Hydrolysis of degradable particulate org. matter</td><td></td></tr>
!> <tr><td></td><td>lysis.f</td><td>Lysis of bacteria</td><td></td></tr>
!> <tr><td></td><td>mfbnut.f</td><td>Microfytobenthos nutrient concentration in the bottom GEM</td><td></td></tr>
!> <tr><td></td><td>minlim.f</td><td>Mineralisation organic C, N, P and Si</td><td></td></tr>
!> <tr><td></td><td>nh3fre.f</td><td>Calculation conc. unionized ammonia</td><td></td></tr>
!> <tr><td></td><td>nitrif.f</td><td>Nitrification of ammonium + decay of CBOD</td><td></td></tr>
!> <tr><td></td><td>nralgs.f</td><td>Nutrient release of algae in S1 and S2</td><td></td></tr>
!> <tr><td></td><td>nutcnk.f</td><td>Nutrient cycle model by Nakata and Kuramoto</td><td></td></tr>
!> <tr><td></td><td>nutrel.f</td><td>Release (nutrients/detritus) by of mortality algae</td><td></td></tr>
!> <tr><td></td><td>nutupt.f</td><td>Uptake of nutrients by growth of algae</td><td></td></tr>
!> <tr><td></td><td>oyster.f</td><td>Oysterfarming as forcing function for primary consumption</td><td></td></tr>
!> <tr><td></td><td>phcomb.f</td><td>Calculates total C, P, N, Si, Dm, Chlorophyll in algae from fractions in Bloom</td><td></td></tr>
!> <tr><td></td><td>ptewor.f</td><td>Production fluxes for TEWOR+</td><td></td></tr>
!> <tr><td></td><td>resant.f</td><td>Resuspension of nutrients in organic carbon matrix</td><td></td></tr>
!> <tr><td></td><td>rescar.f</td><td>Resuspension of particulates by fraction * resdm, from S2 if S1 is exhausted</td><td></td></tr>
!> <tr><td></td><td>resnut.f</td><td>Resuspension (from 2 layers) of nutrients in the organic carbon matrix</td><td></td></tr>
!> <tr><td></td><td>sdppro.f</td><td>Traditional algal growth module (DYNAMO)</td><td></td></tr>
!> <tr><td></td><td>sedaap.f</td><td>Sedimentation flux and velocity for PAP and AAP (adsorbed PO4)</td><td></td></tr>
!> <tr><td></td><td>sedcar.f</td><td>Sedimentation routine used for IMx, OOC, algae, BOD pools, bacteria etc.</td><td></td></tr>
!> <tr><td></td><td>sednal.f</td><td>Sedimentation of nutrients in algae, diatoms</td><td></td></tr>
!> <tr><td></td><td>sednut.f</td><td>Sedimentation of nutrients in the organic carbon matrix (GEM)</td><td></td></tr>
!> <tr><td></td><td>swbura.f</td><td>Sedimentation of nutrients in organic carbon matrix (gx/m2/d => gC/m2/d)</td><td></td></tr>
!> <tr><td></td><td>swburn.f</td><td>Sedimentation of nutrients in organic carbon matrix (gC/m2/d => gx/m2/d)</td><td></td></tr>
!> <tr><td></td><td>swsedn.f</td><td>Sedimentation of nutrients in organic carbon matrix (SWITCH)</td><td></td></tr>
!> <tr><td></td><td>vertnk.f</td><td>Vert.transport TIM for nutr. cycle model (Nakata and Kuramoto)</td><td></td></tr>
!> <tr><td></td><td>vivia2.f</td><td>Dissolution/precipitation of P in vivianite</td><td>an error(?) corrected in vivian</td><td></td></tr>
!> <tr><td></td><td>vivian.f</td><td>Dissolution/precipitation of P in vivianite</td><td>why 2 separate routines ?</td><td></td></tr>
!> <tr><td></td><td>wkcomp.f</td><td>Composition</td><td></td></tr>
!> <tr><td></td><td>zoodyn.f</td><td>Dynamic calculation of grazing and zooplankton biomass</td><td></td></tr>
!> <tr><td>algae growth</td><td></td><td></td><td></td></tr>
!> <tr><td></td><td>algmrt.f</td><td>Algea mortality of greens and diatoms in the bed (S1,S2) DYNAMO</td><td></td></tr>
!> <tr><td></td><td>bluetd.f</td><td></td><td></td></tr>
!> <tr><td></td><td>burcar.f</td><td>Burial of IMx, DetC, OOC, Green, Diat and AAP</td><td></td></tr>
!> <tr><td></td><td>consbl.f</td><td>Grazing module</td><td></td></tr>
!> <tr><td></td><td>d40blo.f</td><td>BLOOM II algae module</td><td></td></tr>
!> <tr><td></td><td>depave.f</td><td>Average depth for a Bloom time step (typically a day)</td><td></td></tr>
!> <tr><td></td><td>digcar.f</td><td>Digging of IMx, DetC, OOC, Green, Diat and AAP</td><td></td></tr>
!> <tr><td></td><td>dlalg.f</td><td>Daylength function for algae DYNAMO</td><td></td></tr>
!> <tr><td></td><td>dmvol.f</td><td>Volume of dry matter in a segment</td><td></td></tr>
!> <tr><td></td><td>extinc.f</td><td>Extinction formulae ( inorganic , organic and algae )</td><td></td></tr>
!> <tr><td></td><td>gemmfb.f</td><td>GEM microfytobenthos production</td><td></td></tr>
!> <tr><td></td><td>gemmnd.f</td><td>GEM algae primary production module</td><td></td></tr>
!> <tr><td></td><td>gemnlm.f</td><td>GEM algae nutrient limitation function</td><td></td></tr>
!> <tr><td></td><td>gemtmp.f</td><td>GEM algae temperature limitation function</td><td></td></tr>
!> <tr><td></td><td>gmreed.f</td><td>Growth, mortality and decay of reed as helofyte filter</td><td></td></tr>
!> <tr><td></td><td>mfbllm.f</td><td>Microfytobenthos light limitation function</td><td></td></tr>
!> <tr><td></td><td>mndini.f</td><td>Initialisation MND algae GEM</td><td></td></tr>
!> <tr><td></td><td>mndllm.f</td><td>MND algae light limitation function GEM</td><td></td></tr>
!> <tr><td></td><td>nlalg.f</td><td>Nutrient limiation function for green algae</td><td></td></tr>
!> <tr><td></td><td>oyster.f</td><td>Oysterfarming as forcing function for primary consumption</td><td></td></tr>
!> <tr><td></td><td>phcomb.f</td><td>Calculates total C, P, N, Si, Dm, Chlorophyll in algae from fractions in Bloom</td><td></td></tr>
!> <tr><td></td><td>phcomp.f</td><td>Composition of phytoplankton by summing algae fractions - Dynamo - GEM</td><td></td></tr>
!> <tr><td></td><td>pocomp.f</td><td>Composition of POC by summing algae fractions Dynamo, Bloom and GEM</td><td></td></tr>
!> <tr><td></td><td>pprlim.f</td><td>Limitation (numerical) on primary production DYNAMO</td><td></td></tr>
!> <tr><td></td><td>pripro.f</td><td>Nett primary production and mortality DYNAMO algae</td><td></td></tr>
!> <tr><td></td><td>radalg.f</td><td>Light efficiency function DYNAMO algae</td><td></td></tr>
!> <tr><td></td><td>rdbalg.f</td><td>Light efficiency function DYNAMO algae</td><td></td></tr>
!> <tr><td></td><td>resalg.f</td><td>Resuspension of algae using resdm * fraction</td><td></td></tr>
!> <tr><td></td><td>ptewor.f</td><td>Production fluxes for TEWOR+</td><td></td></tr>
!> <tr><td></td><td>rescar.f</td><td>Resuspension of particulates by fraction * resdm, from S2 if S1 is exhausted</td><td></td></tr>
!> <tr><td></td><td>sdppro.f</td><td>Nett primary production and mortality diatoms</td><td></td></tr>
!> <tr><td></td><td>secchi.f</td><td>Extinction of visible-light (370-680nm) by inorganic, organic and algae</td><td></td></tr>
!> <tr><td></td><td>sednal.f</td><td>Sedimentation of nutrients in algae, diatoms</td><td></td></tr>
!> <tr><td></td><td>sedphy.f</td><td>Calculates the settling velocity of algae (BLOOM)</td><td></td></tr>
!> <tr><td></td><td>ssedph.f</td><td>Sum of sedimentation flux of algae Dynamo - Bloom - GEM </td><td></td></tr>
!> <tr><td></td><td>stvar.f</td><td>Dummy for consbl to save the state variables</td><td></td></tr>
!> <tr><td></td><td>swbur.f</td><td>Fluxes in the water bed due to burial and digging</td><td></td></tr>
!> <tr><td></td><td>swbura.f</td><td>Burial nutrients in diatoms from sediment S1</td><td></td></tr>
!> <tr><td></td><td>tfalg.f</td><td>Temperature functions for algae growth and mortality</td><td></td></tr>
!> <tr><td></td><td>ulfix.f</td><td>Fixation of BLOOM algae at the water bed (e.g. for Ulvae)</td><td></td></tr>
!> <tr><td></td><td>varoxy.f</td><td>Variation of primary production within day</td><td></td></tr>
!> <tr><td></td><td>vtrans.f</td><td>Vertical distribution after a longer time span to correct 3D-BLOOM</td><td></td></tr>
!> <tr><td></td><td>wkcomp.f</td><td>Composition</td><td></td></tr>
!> <tr><td></td><td>zoodyn.f</td><td>Grazing module</td><td></td></tr>
!> <tr><td>Micro pollutants</td><td></td><td></td><td></td></tr>
!> <tr><td></td><td>burhm.f</td><td>Burial of all heavy metals</td><td></td></tr>
!> <tr><td></td><td>buromv.f</td><td>Burial of all organic micros from S1 and S2</td><td></td></tr>
!> <tr><td></td><td>cec.f</td><td>Calculation Cation Exchange Capacity</td><td></td></tr>
!> <tr><td></td><td>degmp.f</td><td>Degradation of organic micropolutants</td><td></td></tr>
!> <tr><td></td><td>digfix.f</td><td>Digging flux of reversible sorbed HM/OMP</td><td></td></tr>
!> <tr><td></td><td>dighm.f</td><td>Digging of all heavy metals</td><td></td></tr>
!> <tr><td></td><td>digomv.f</td><td>Digging of all organic micros towards S1 and S2</td><td></td></tr>
!> <tr><td></td><td>ksorhm.f</td><td>Slow adsorption and desorption of mive's. 3 compartment model instantaneous + 2 first order</td><td></td></tr>
!> <tr><td></td><td>ksorom.f</td><td>Slow adsorption and desorption of omive's. 3 compartment model instantaneous + 2 first order </td><td></td></tr>
!> <tr><td></td><td>metres.f</td><td>Resuspension of metals for western scheldt estuary</td><td></td></tr>
!> <tr><td></td><td>norhm.f</td><td>Normalisation of heavy metal content of the total solid</td><td></td></tr>
!> <tr><td></td><td>partmp.f</td><td>Partitioning of micropollutants</td><td></td></tr>
!> <tr><td></td><td>resfix.f</td><td>Resuspension fluxes for HM/OMP are multiplied with fraction fixed HM/OMP</td><td></td></tr>
!> <tr><td></td><td>reshm.f</td><td>Resuspension of adsorbed heavy metal</td><td></td></tr>
!> <tr><td></td><td>resomv.f</td><td>Resuspension adsorbed organic micro pollutants</td><td></td></tr>
!> <tr><td></td><td>rfpart.f</td><td>Reprofunctions for HM partition coefficients</td><td></td></tr>
!> <tr><td></td><td>sedfix.f</td><td>Multiplication sedimentatio fluxes HM/OMP with fraction fixed HM/OMP</td><td></td></tr>
!> <tr><td></td><td>sedhm.f</td><td>Sedimentation flux and velocity of adsorbed heavy metals</td><td></td></tr>
!> <tr><td></td><td>sedomv.f</td><td>Sedimentation flux and velocity of adsorbed organic micro pollutants</td><td></td></tr>
!> <tr><td></td><td>suloxi.f</td><td>Decay of metal sulfides for western scheldt estuary</td><td></td></tr>
!> <tr><td></td><td>sulsed.f</td><td>Sedimentation of metal sulfides for Western Scheldt estuary</td><td></td></tr>
!> <tr><td></td><td>swoxy.f</td><td>Partitioning switch in WC, S1 and S2 based on actual and critical oxygen concentration</td><td></td></tr>
!> <tr><td></td><td>trcoef.f</td><td>Gas and liquid exchange organic micro pollutants (Lyman and O'Connor)</td><td></td></tr>
!> <tr><td></td><td>vervlu.f</td><td>Atmospheric exch. OMPs (volatilization/intake)</td><td></td></tr>
!> <tr><td></td><td>watmin.f</td><td>General mineralisation routine 0+1st order, temperature corrected</td><td></td></tr>
!> <tr><td>Physics</td><td></td><td></td><td></td></tr>
!> <tr><td></td><td>calchz.f</td><td>Calculate chezy coefficient using roughness and depth</td><td></td></tr>
!> <tr><td></td><td>caltau.f</td><td>Calculation of bottom friction<td></td><td></td></tr>
!> <tr><td></td><td>calvs.f</td><td>Calculate individual sedimentation velocities</td><td></td></tr>
!> <tr><td></td><td>calwav.f</td><td>Wave characteristics</td><td></td></tr>
!> <tr><td></td><td>chlor.f</td><td>Calculation of chloride from salinity</td><td></td></tr>
!> <tr><td></td><td>clcrad.f</td><td>Radiation at segment upper and lower boundaries</td><td></td></tr>
!> <tr><td></td><td>dayl.f</td><td>Daylength calculation</td><td></td></tr>
!> <tr><td></td><td>ddelt.f</td><td>Scaling of model DT to aux time step</td><td></td></tr>
!> <tr><td></td><td>ddepth.f</td><td>Dynamic calculation of the depth</td><td></td></tr>
!> <tr><td></td><td>dsptra.f</td><td>Dispersion/diffusion in the sediment</td><td></td></tr>
!> <tr><td></td><td>dsurf.f</td><td>Dynamic calculation of the horizontal surface area from volume and depth</td><td></td></tr>
!> <tr><td></td><td>emersi.f</td><td>emersion of segments in z-layers, set segment features accordingly</td><td></td></tr>
!> <tr><td></td><td>extina.f</td><td>Extinction of light by algae and POC</td><td></td></tr>
!> <tr><td></td><td>extinc.f</td><td>Extinction formulae ( inorganic , organic and algae )</td><td></td></tr>
!> <tr><td></td><td>grd.f90</td><td>gradient in density or in the horizontal stream velocity </td><td></td></tr>
!> <tr><td></td><td>hdisp.f</td><td>(1D) Horizontal dispersion as velocity dependent reprofunction</td><td></td></tr>
!> <tr><td></td><td>hdispv.f</td><td>(2D) Horizontal dispersion as velocity dependent reprofunction</td><td></td></tr>
!> <tr><td></td><td>heatfl.f</td><td>Total heat flux for surface water absolute temperature model</td><td></td></tr>
!> <tr><td></td><td>intpol.f</td><td>Depth where wave is created or wind fetch from wind direction</td><td></td></tr>
!> <tr><td></td><td>linpol.f</td><td>Linear interpolation q-h relation</td><td></td></tr>
!> <tr><td></td><td>meteo.f</td><td>Process meteo from various meteo-stations</td><td></td></tr>
!> <tr><td></td><td>s2x.f90</td><td>Conversion of variables per volume to per exchange by linear interpolation)</td><td></td></tr>
!> <tr><td></td><td>stox3d.f</td><td>Vertical dispersion (segment -> exchange)</td><td></td></tr>
!> <tr><td></td><td>temper.f</td><td>Exchange of excess temperature at the surface (Sweers)</td><td></td></tr>
!> <tr><td></td><td>totdep.f</td><td>Total depth water column</td><td></td></tr>
!> <tr><td></td><td>varsal.f</td><td>salinity in case of constant river discharge</td><td></td></tr>
!> <tr><td></td><td>vdisp.f</td><td>Vertical dispersion in a stratified water body using Richardson's formulae</td><td></td></tr>
!> <tr><td></td><td>veloc.f90</td><td>Horizontal stream velocity in a segment based on flows at interfaces</td><td></td></tr>
!> <tr><td></td><td>velocv.f90</td><td>Horizontal stream velocity in a segment based on velocities at interfaces</td><td></td></tr>
!> <tr><td></td><td>xtos3d.f</td><td>Conversion of units on interfaces to volumes for 3rd direction only</td><td></td></tr>
!> <tr><td></td><td>xveloc.f</td><td>Calculation of velocity's on exchanges from flows</td><td></td></tr>
!> <tr><td>bed behaviour</td><td></td><td></td><td></td></tr>
!> <tr><td></td><td>advtra.f</td><td>Advective transport, velocities and fluxes, of solids in sediment</td><td></td></tr>
!> <tr><td></td><td>burial.f</td><td>Burial total bottom mass (dry matter)</td><td></td></tr>
!> <tr><td></td><td>burcar.f</td><td>Burial of IMx, DetC, OOC, Green, Diat and AAP</td><td></td></tr>
!> <tr><td></td><td>calsnd.f</td><td>Transport of non-cohesive sediment</td><td></td></tr>
!> <tr><td></td><td>digcar.f</td><td>Digging of IMx, DetC, OOC, Green, Diat and AAP</td><td></td></tr>
!> <tr><td></td><td>diggin.f</td><td>Digging dry matter to sediment S1 and S2</td><td></td></tr>
!> <tr><td></td><td>rescar.f</td><td>Resuspension of particulates by fraction * resdm, from S2 if S1 is exhausted</td><td></td></tr>
!> <tr><td></td><td>sedcar.f</td><td>Sedimentation routine used for IMx, OOC, algae, BOD pools, bacteria etc.</td><td></td></tr>
!> <tr><td></td><td>sedcom.f</td><td>Composition, thickness, total dry mass and density in sediment layers</td><td></td></tr>
!> <tr><td></td><td>somres.f</td><td>Sum of (in)organic substances resusp./buried/digged from S1/S2</td><td></td></tr>
!> <tr><td></td><td>somsed.f</td><td>Total of all sedimenting substances</td><td></td></tr>
!> <tr><td></td><td>trase2.f</td><td>Total of transport in sediment for 66 substances</td><td></td></tr>
!> <tr><td>chemistry</td><td></td><td></td><td></td></tr>
!> <tr><td></td><td>cselac.f</td><td>Consumption of electron acceptors</td><td></td></tr>
!> <tr><td></td><td>d40cha.f</td><td>CHARON chemical equilibrium module</td><td></td></tr>
!> <tr><td></td><td>ebuch4.f</td><td>Ebullition of methane</td><td></td></tr>
!> <tr><td></td><td>feadso.f</td><td>Computes Fe adsorbens for P for WESTERN SCHELDT ESTUARY only!!</td><td></td></tr>
!> <tr><td></td><td>resdm.f</td><td>Resuspension total bottom material (dry mass)</td><td></td></tr>
!> <tr><td></td><td>salin.f</td><td>Converts chloride concentration to salinity (Aquatic Chemistry p 567)</td><td></td></tr>
!> <tr><td></td><td>satch4.f</td><td>Methane saturation concentration based on atmospheric methane pressure</td><td></td></tr>
!> <tr><td></td><td>simph.f</td><td>Simple calculation of pH</td><td></td></tr>
!> <tr><td></td><td>sulfid.f</td><td>Speciation of dissolved sulphide (S= and HS-) in pore water</td><td></td></tr>
!> <tr><td></td><td>sulfox.f</td><td>Oxidation of dissolved sulphide (0 and 2nd order) (new, generic !)</td><td></td></tr>
!> <tr><td></td><td>sulfpr.f</td><td>Precipitation and dissolution of sulphide as first order process</td><td></td></tr>
!> </table>
