% Generated by ADiMat 0.6.0-4867
% © 2001-2008 Andre Vehreschild <vehreschild@sc.rwth-aachen.de>
% © 2009-2013 Johannes Willkomm <johannes.willkomm@sc.tu-darmstadt.de>
% RWTH Aachen University, 52056 Aachen, Germany
% TU Darmstadt, 64289 Darmstadt, Germany
% Visit us on the web at http://www.adimat.de/
% Report bugs to adimat-users@lists.sc.informatik.tu-darmstadt.de
%
%                             DISCLAIMER
% 
% ADiMat was prepared as part of an employment at the Institute for Scientific Computing,
% RWTH Aachen University, Germany and at the Institute for Scientific Computing,
% TU Darmstadt, Germany and is provided AS IS. 
% NEITHER THE AUTHOR(S), THE GOVERNMENT OF THE FEDERAL REPUBLIC OF GERMANY
% NOR ANY AGENCY THEREOF, NOR THE RWTH AACHEN UNIVERSITY, NOT THE TU DARMSTADT,
% INCLUDING ANY OF THEIR EMPLOYEES OR OFFICERS, MAKES ANY WARRANTY, EXPRESS OR IMPLIED,
% OR ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS,
% OR USEFULNESS OF ANY INFORMATION OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE
% WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS.
%
% Flags: BACKWARDMODE,  NOOPEROPTIM,
%   NOLOCALCSE,  NOGLOBALCSE,  NOPRESCALARFOLDING,
%   NOPOSTSCALARFOLDING,  NOCONSTFOLDMULT0,  FUNCMODE,
%   NOTMPCLEAR,  DUMP_XML,  PARSE_ONLY,
%   UNBOUND_ERROR
%
% Parameters:
%  - dependents=Q, R
%  - independents=A
%  - inputEncoding=ISO-8859-1
%  - output-mode: plain
%  - output-file: ad_out/a_adimat_qr.m
%  - output-file-prefix: 
%  - output-directory: ad_out
% Generated by ADiMat 0.6.0-4867
% © 2001-2008 Andre Vehreschild <vehreschild@sc.rwth-aachen.de>
% © 2009-2013 Johannes Willkomm <johannes.willkomm@sc.tu-darmstadt.de>
% RWTH Aachen University, 52056 Aachen, Germany
% TU Darmstadt, 64289 Darmstadt, Germany
% Visit us on the web at http://www.adimat.de/
% Report bugs to adimat-users@lists.sc.informatik.tu-darmstadt.de
%
%                             DISCLAIMER
% 
% ADiMat was prepared as part of an employment at the Institute for Scientific Computing,
% RWTH Aachen University, Germany and at the Institute for Scientific Computing,
% TU Darmstadt, Germany and is provided AS IS. 
% NEITHER THE AUTHOR(S), THE GOVERNMENT OF THE FEDERAL REPUBLIC OF GERMANY
% NOR ANY AGENCY THEREOF, NOR THE RWTH AACHEN UNIVERSITY, NOT THE TU DARMSTADT,
% INCLUDING ANY OF THEIR EMPLOYEES OR OFFICERS, MAKES ANY WARRANTY, EXPRESS OR IMPLIED,
% OR ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS,
% OR USEFULNESS OF ANY INFORMATION OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE
% WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS.
%
% Flags: BACKWARDMODE,  NOOPEROPTIM,
%   NOLOCALCSE,  NOGLOBALCSE,  NOPRESCALARFOLDING,
%   NOPOSTSCALARFOLDING,  NOCONSTFOLDMULT0,  FUNCMODE,
%   NOTMPCLEAR,  DUMP_XML,  PARSE_ONLY,
%   UNBOUND_ERROR
%
% Parameters:
%  - dependents=Q, R
%  - independents=A
%  - inputEncoding=ISO-8859-1
%  - output-mode: plain
%  - output-file: ad_out/a_adimat_qr.m
%  - output-file-prefix: 
%  - output-directory: ad_out
%
% Functions in this file: a_adimat_qr, rec_adimat_qr,
%  ret_adimat_qr, a_mk_householder_elim_vec_lapack, rec_mk_householder_elim_vec_lapack,
%  ret_mk_householder_elim_vec_lapack
%

function [a_A nr_Q nr_R] = a_adimat_qr(A, a_Q, a_R)
   Pk = 0;
   [m n] = size(A);
   r = min(m, n);
   Q = eye(m);
   tmpba1 = 0;
   if m<=n && isreal(A)
      tmpba1 = 1;
      adimat_push1(r);
      r = r - 1;
   end
   adimat_push1(tmpba1);
   tmpfra1_2 = r;
   for k=1 : tmpfra1_2
      adimat_push1(Pk);
      Pk = rec_mk_householder_elim_vec_lapack(A(k : m, k), m);
      adimat_push1(Q);
      Q = Q * Pk;
      adimat_push1(A);
      A = Pk' * A;
   end
   adimat_push1(tmpfra1_2);
   R = triu(A);
   nr_Q = Q;
   nr_R = R;
   [a_Pk a_A] = a_zeros(Pk, A);
   if nargin < 2
      a_Q = a_zeros1(Q);
   end
   if nargin < 3
      a_R = a_zeros1(R);
   end
   a_A = adimat_adjsum(a_A, call(@triu, a_R));
   tmpfra1_2 = adimat_pop1;
   for k=fliplr(1 : tmpfra1_2)
      A = adimat_pop1;
      a_Pk = adimat_adjsum(a_Pk, a_ctranspose(adimat_adjmultl(Pk', a_A, A), Pk));
      tmpsa1 = a_A;
      a_A = a_zeros1(A);
      a_A = adimat_adjsum(a_A, adimat_adjmultr(A, Pk', tmpsa1));
      Q = adimat_pop1;
      a_Pk = adimat_adjsum(a_Pk, adimat_adjmultr(Pk, Q, a_Q));
      tmpsa1 = a_Q;
      a_Q = a_zeros1(Q);
      a_Q = adimat_adjsum(a_Q, adimat_adjmultl(Q, tmpsa1, Pk));
      [tmpadjc1] = ret_mk_householder_elim_vec_lapack(a_Pk);
      Pk = adimat_pop1;
      a_A(k : m, k) = adimat_adjsum(a_A(k : m, k), tmpadjc1);
      a_Pk = a_zeros1(Pk);
   end
   tmpba1 = adimat_pop1;
   if tmpba1 == 1
      r = adimat_pop1;
   end
end

function [Q R] = rec_adimat_qr(A)
   Pk = 0;
   [m n] = size(A);
   r = min(m, n);
   Q = eye(m);
   tmpba1 = 0;
   if m<=n && isreal(A)
      tmpba1 = 1;
      adimat_push1(r);
      r = r - 1;
   end
   adimat_push1(tmpba1);
   tmpfra1_2 = r;
   for k=1 : tmpfra1_2
      adimat_push1(Pk);
      Pk = rec_mk_householder_elim_vec_lapack(A(k : m, k), m);
      adimat_push1(Q);
      Q = Q * Pk;
      adimat_push1(A);
      A = Pk' * A;
   end
   adimat_push1(tmpfra1_2);
   R = triu(A);
   adimat_push(Pk, m, n, r, Q, R, A);
end

function a_A = ret_adimat_qr(a_Q, a_R)
   [A R Q r n m Pk] = adimat_pop;
   [a_Pk a_A] = a_zeros(Pk, A);
   if nargin < 1
      a_Q = a_zeros1(Q);
   end
   if nargin < 2
      a_R = a_zeros1(R);
   end
   a_A = adimat_adjsum(a_A, call(@triu, a_R));
   tmpfra1_2 = adimat_pop1;
   for k=fliplr(1 : tmpfra1_2)
      A = adimat_pop1;
      a_Pk = adimat_adjsum(a_Pk, a_ctranspose(adimat_adjmultl(Pk', a_A, A), Pk));
      tmpsa1 = a_A;
      a_A = a_zeros1(A);
      a_A = adimat_adjsum(a_A, adimat_adjmultr(A, Pk', tmpsa1));
      Q = adimat_pop1;
      a_Pk = adimat_adjsum(a_Pk, adimat_adjmultr(Pk, Q, a_Q));
      tmpsa1 = a_Q;
      a_Q = a_zeros1(Q);
      a_Q = adimat_adjsum(a_Q, adimat_adjmultl(Q, tmpsa1, Pk));
      [tmpadjc1] = ret_mk_householder_elim_vec_lapack(a_Pk);
      Pk = adimat_pop1;
      a_A(k : m, k) = adimat_adjsum(a_A(k : m, k), tmpadjc1);
      a_Pk = a_zeros1(Pk);
   end
   tmpba1 = adimat_pop1;
   if tmpba1 == 1
      r = adimat_pop1;
   end
end
% $Id: adimat_qr.m 3925 2013-10-14 12:09:14Z willkomm $

function [a_a nr_Pk nr_u] = a_mk_householder_elim_vec_lapack(a, n, a_Pk, a_u)
   tmplia1 = 0;
   tmpca1 = 0;
   tmpca3 = 0;
   tmpda2 = 0;
   u = 0;
   na_rest = 0;
   sa1 = 0;
   nu = 0;
   sigma = 0;
   Pksub = 0;
   tolZ = eps;
   assert(iscolumn(a));
   tmpca2 = a(1) .* 0;
   tmpda1 = eye(n);
   Pk = tmpda1 + tmpca2;
   k = length(a);
   na = norm(a);
   tmpba1 = 0;
   if ~(k==1 && isreal(a)) && na~=0
      tmpba1 = 1;
      adimat_push1(u);
      u = a;
      adimat_push1(na_rest);
      na_rest = norm(a(2 : end));
      tmpba2 = 0;
      if na>tolZ && na_rest~=0
         tmpba2 = 1;
         adimat_push1(sa1);
         sa1 = sign(real(a(1)));
         tmpba3 = 0;
         if sa1 == 0
            tmpba3 = 1;
            adimat_push1(sa1);
            sa1 = 1;
         end
         adimat_push(tmpba3, nu);
         nu = sa1 .* na;
         adimat_push1(tmplia1);
         tmplia1 = u(1) + nu;
         adimat_push_index1(u, 1);
         u(1) = tmplia1;
         adimat_push1(tmpca1);
         tmpca1 = a(1) + nu;
         adimat_push1(u);
         u = u ./ tmpca1;
         adimat_push1(tmpca1);
         tmpca1 = a(1) + nu;
         adimat_push1(sigma);
         sigma = tmpca1 ./ nu;
         adimat_push1(tmpca3);
         tmpca3 = sigma .* u;
         adimat_push1(tmpca2);
         tmpca2 = tmpca3 * u';
         adimat_push1(tmpda1);
         tmpda1 = eye(k);
         adimat_push1(Pksub);
         Pksub = tmpda1 - tmpca2;
         adimat_push1(tmpda2);
         tmpda2 = n - k + 1;
         adimat_push1(tmpda1);
         tmpda1 = n - k + 1;
         adimat_push_index2(Pk, tmpda1 : adimat_end(Pk, 1, 2), tmpda2 : adimat_end(Pk, 2, 2));
         Pk(tmpda1 : end, tmpda2 : end) = Pksub;
      end
      adimat_push1(tmpba2);
   end
   adimat_push1(tmpba1);
   nr_Pk = Pk;
   nr_u = u;
   [a_nu a_sigma a_Pksub a_tmpca2 a_na a_tmplia1 a_tmpca1 a_tmpca3 a_a] = a_zeros(nu, sigma, Pksub, tmpca2, na, tmplia1, tmpca1, tmpca3, a);
   if nargin < 3
      a_Pk = a_zeros1(Pk);
   end
   if nargin < 4
      a_u = a_zeros1(u);
   end
   tmpba1 = adimat_pop1;
   if tmpba1 == 1
      tmpba2 = adimat_pop1;
      if tmpba2 == 1
         Pk = adimat_pop_index2(Pk, tmpda1 : adimat_end(Pk, 1, 2), tmpda2 : adimat_end(Pk, 2, 2));
         a_Pksub = adimat_adjsum(a_Pksub, adimat_adjred(Pksub, adimat_adjreshape(Pksub, a_Pk(tmpda1 : end, tmpda2 : end))));
         a_Pk = a_zeros_index2(a_Pk, Pk, tmpda1 : adimat_end(Pk, 1, 2), tmpda2 : adimat_end(Pk, 2, 2));
         [tmpda1 tmpda2 Pksub] = adimat_pop;
         a_tmpca2 = adimat_adjsum(a_tmpca2, adimat_adjred(tmpca2, -a_Pksub));
         a_Pksub = a_zeros1(Pksub);
         [tmpda1 tmpca2] = adimat_pop;
         a_tmpca3 = adimat_adjsum(a_tmpca3, adimat_adjmultl(tmpca3, a_tmpca2, u'));
         a_u = adimat_adjsum(a_u, a_ctranspose(adimat_adjmultr(u', tmpca3, a_tmpca2), u));
         a_tmpca2 = a_zeros1(tmpca2);
         tmpca3 = adimat_pop1;
         a_sigma = adimat_adjsum(a_sigma, adimat_adjred(sigma, a_tmpca3 .* u));
         a_u = adimat_adjsum(a_u, adimat_adjred(u, sigma .* a_tmpca3));
         a_tmpca3 = a_zeros1(tmpca3);
         sigma = adimat_pop1;
         a_tmpca1 = adimat_adjsum(a_tmpca1, adimat_adjred(tmpca1, a_sigma ./ nu));
         a_nu = adimat_adjsum(a_nu, adimat_adjred(nu, -((tmpca1./nu .* a_sigma) ./ nu)));
         a_sigma = a_zeros1(sigma);
         tmpca1 = adimat_pop1;
         a_a(1) = adimat_adjsum(a_a(1), adimat_adjred(a(1), a_tmpca1));
         a_nu = adimat_adjsum(a_nu, adimat_adjred(nu, a_tmpca1));
         a_tmpca1 = a_zeros1(tmpca1);
         u = adimat_pop1;
         a_tmpca1 = adimat_adjsum(a_tmpca1, adimat_adjred(tmpca1, -((u./tmpca1 .* a_u) ./ tmpca1)));
         tmpsa1 = a_u;
         a_u = a_zeros1(u);
         a_u = adimat_adjsum(a_u, adimat_adjred(u, tmpsa1 ./ tmpca1));
         tmpca1 = adimat_pop1;
         a_a(1) = adimat_adjsum(a_a(1), adimat_adjred(a(1), a_tmpca1));
         a_nu = adimat_adjsum(a_nu, adimat_adjred(nu, a_tmpca1));
         a_tmpca1 = a_zeros1(tmpca1);
         u = adimat_pop_index1(u, 1);
         a_tmplia1 = adimat_adjsum(a_tmplia1, adimat_adjred(tmplia1, adimat_adjreshape(tmplia1, a_u(1))));
         a_u = a_zeros_index1(a_u, u, 1);
         tmplia1 = adimat_pop1;
         a_u(1) = adimat_adjsum(a_u(1), adimat_adjred(u(1), a_tmplia1));
         a_nu = adimat_adjsum(a_nu, adimat_adjred(nu, a_tmplia1));
         a_tmplia1 = a_zeros1(tmplia1);
         nu = adimat_pop1;
         a_na = adimat_adjsum(a_na, adimat_adjred(na, sa1 .* a_nu));
         a_nu = a_zeros1(nu);
         tmpba3 = adimat_pop1;
         if tmpba3 == 1
            sa1 = adimat_pop1;
         end
         sa1 = adimat_pop1;
      end
      [na_rest u] = adimat_pop;
      a_a = adimat_adjsum(a_a, a_u);
      a_u = a_zeros1(u);
   end
   a_a = adimat_adjsum(a_a, a_adimat_norm1(a, a_na));
   a_tmpca2 = adimat_adjsum(a_tmpca2, adimat_adjred(tmpca2, a_Pk));
   a_a(1) = adimat_adjsum(a_a(1), adimat_adjred(a(1), a_tmpca2 .* 0));
   assert(iscolumn(a));
end

function [Pk u] = rec_mk_householder_elim_vec_lapack(a, n)
   tmplia1 = 0;
   tmpca1 = 0;
   tmpca3 = 0;
   tmpda2 = 0;
   u = 0;
   na_rest = 0;
   sa1 = 0;
   nu = 0;
   sigma = 0;
   Pksub = 0;
   tolZ = eps;
   assert(iscolumn(a));
   tmpca2 = a(1) .* 0;
   tmpda1 = eye(n);
   Pk = tmpda1 + tmpca2;
   k = length(a);
   na = norm(a);
   tmpba1 = 0;
   if ~(k==1 && isreal(a)) && na~=0
      tmpba1 = 1;
      adimat_push1(u);
      u = a;
      adimat_push1(na_rest);
      na_rest = norm(a(2 : end));
      tmpba2 = 0;
      if na>tolZ && na_rest~=0
         tmpba2 = 1;
         adimat_push1(sa1);
         sa1 = sign(real(a(1)));
         tmpba3 = 0;
         if sa1 == 0
            tmpba3 = 1;
            adimat_push1(sa1);
            sa1 = 1;
         end
         adimat_push(tmpba3, nu);
         nu = sa1 .* na;
         adimat_push1(tmplia1);
         tmplia1 = u(1) + nu;
         adimat_push_index1(u, 1);
         u(1) = tmplia1;
         adimat_push1(tmpca1);
         tmpca1 = a(1) + nu;
         adimat_push1(u);
         u = u ./ tmpca1;
         adimat_push1(tmpca1);
         tmpca1 = a(1) + nu;
         adimat_push1(sigma);
         sigma = tmpca1 ./ nu;
         adimat_push1(tmpca3);
         tmpca3 = sigma .* u;
         adimat_push1(tmpca2);
         tmpca2 = tmpca3 * u';
         adimat_push1(tmpda1);
         tmpda1 = eye(k);
         adimat_push1(Pksub);
         Pksub = tmpda1 - tmpca2;
         adimat_push1(tmpda2);
         tmpda2 = n - k + 1;
         adimat_push1(tmpda1);
         tmpda1 = n - k + 1;
         adimat_push_index2(Pk, tmpda1 : adimat_end(Pk, 1, 2), tmpda2 : adimat_end(Pk, 2, 2));
         Pk(tmpda1 : end, tmpda2 : end) = Pksub;
      end
      adimat_push1(tmpba2);
   end
   adimat_push(tmpba1, na_rest, sa1, nu, sigma, Pksub, tolZ, tmpca2, tmpda1, k, na, tmplia1, tmpca1, tmpca3, tmpda2, Pk, u, a);
   if nargin > 1
      adimat_push1(n);
   end
   adimat_push1(nargin);
end

function a_a = ret_mk_householder_elim_vec_lapack(a_Pk, a_u)
   tmpnargin = adimat_pop1;
   if tmpnargin > 1
      n = adimat_pop1;
   end
   [a u Pk tmpda2 tmpca3 tmpca1 tmplia1 na k tmpda1 tmpca2 tolZ Pksub sigma nu sa1 na_rest] = adimat_pop;
   [a_nu a_sigma a_Pksub a_tmpca2 a_na a_tmplia1 a_tmpca1 a_tmpca3 a_a] = a_zeros(nu, sigma, Pksub, tmpca2, na, tmplia1, tmpca1, tmpca3, a);
   if nargin < 1
      a_Pk = a_zeros1(Pk);
   end
   if nargin < 2
      a_u = a_zeros1(u);
   end
   tmpba1 = adimat_pop1;
   if tmpba1 == 1
      tmpba2 = adimat_pop1;
      if tmpba2 == 1
         Pk = adimat_pop_index2(Pk, tmpda1 : adimat_end(Pk, 1, 2), tmpda2 : adimat_end(Pk, 2, 2));
         a_Pksub = adimat_adjsum(a_Pksub, adimat_adjred(Pksub, adimat_adjreshape(Pksub, a_Pk(tmpda1 : end, tmpda2 : end))));
         a_Pk = a_zeros_index2(a_Pk, Pk, tmpda1 : adimat_end(Pk, 1, 2), tmpda2 : adimat_end(Pk, 2, 2));
         [tmpda1 tmpda2 Pksub] = adimat_pop;
         a_tmpca2 = adimat_adjsum(a_tmpca2, adimat_adjred(tmpca2, -a_Pksub));
         a_Pksub = a_zeros1(Pksub);
         [tmpda1 tmpca2] = adimat_pop;
         a_tmpca3 = adimat_adjsum(a_tmpca3, adimat_adjmultl(tmpca3, a_tmpca2, u'));
         a_u = adimat_adjsum(a_u, a_ctranspose(adimat_adjmultr(u', tmpca3, a_tmpca2), u));
         a_tmpca2 = a_zeros1(tmpca2);
         tmpca3 = adimat_pop1;
         a_sigma = adimat_adjsum(a_sigma, adimat_adjred(sigma, a_tmpca3 .* u));
         a_u = adimat_adjsum(a_u, adimat_adjred(u, sigma .* a_tmpca3));
         a_tmpca3 = a_zeros1(tmpca3);
         sigma = adimat_pop1;
         a_tmpca1 = adimat_adjsum(a_tmpca1, adimat_adjred(tmpca1, a_sigma ./ nu));
         a_nu = adimat_adjsum(a_nu, adimat_adjred(nu, -((tmpca1./nu .* a_sigma) ./ nu)));
         a_sigma = a_zeros1(sigma);
         tmpca1 = adimat_pop1;
         a_a(1) = adimat_adjsum(a_a(1), adimat_adjred(a(1), a_tmpca1));
         a_nu = adimat_adjsum(a_nu, adimat_adjred(nu, a_tmpca1));
         a_tmpca1 = a_zeros1(tmpca1);
         u = adimat_pop1;
         a_tmpca1 = adimat_adjsum(a_tmpca1, adimat_adjred(tmpca1, -((u./tmpca1 .* a_u) ./ tmpca1)));
         tmpsa1 = a_u;
         a_u = a_zeros1(u);
         a_u = adimat_adjsum(a_u, adimat_adjred(u, tmpsa1 ./ tmpca1));
         tmpca1 = adimat_pop1;
         a_a(1) = adimat_adjsum(a_a(1), adimat_adjred(a(1), a_tmpca1));
         a_nu = adimat_adjsum(a_nu, adimat_adjred(nu, a_tmpca1));
         a_tmpca1 = a_zeros1(tmpca1);
         u = adimat_pop_index1(u, 1);
         a_tmplia1 = adimat_adjsum(a_tmplia1, adimat_adjred(tmplia1, adimat_adjreshape(tmplia1, a_u(1))));
         a_u = a_zeros_index1(a_u, u, 1);
         tmplia1 = adimat_pop1;
         a_u(1) = adimat_adjsum(a_u(1), adimat_adjred(u(1), a_tmplia1));
         a_nu = adimat_adjsum(a_nu, adimat_adjred(nu, a_tmplia1));
         a_tmplia1 = a_zeros1(tmplia1);
         nu = adimat_pop1;
         a_na = adimat_adjsum(a_na, adimat_adjred(na, sa1 .* a_nu));
         a_nu = a_zeros1(nu);
         tmpba3 = adimat_pop1;
         if tmpba3 == 1
            sa1 = adimat_pop1;
         end
         sa1 = adimat_pop1;
      end
      [na_rest u] = adimat_pop;
      a_a = adimat_adjsum(a_a, a_u);
      a_u = a_zeros1(u);
   end
   a_a = adimat_adjsum(a_a, a_adimat_norm1(a, a_na));
   a_tmpca2 = adimat_adjsum(a_tmpca2, adimat_adjred(tmpca2, a_Pk));
   a_a(1) = adimat_adjsum(a_a(1), adimat_adjred(a(1), a_tmpca2 .* 0));
   assert(iscolumn(a));
end
% $Id: mk_householder_elim_vec_lapack.m 4801 2014-10-08 12:28:59Z willkomm $
