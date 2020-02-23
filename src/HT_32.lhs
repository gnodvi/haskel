================================================================================
                ������� ������� ��������������� ����������������

                ���� ����
                John Hughes - Chalmers University of Technology
================================================================================
  
> module JH_32 where

-- /////////////////////////////////////////////////////////////////////////////

3.2.	��������� �����������������

�� �������� ������������ iterate ��� ��������� ������������������ ����������� � ����������� 
����������� �����. ������� ��, ����� ����������� ������������ 
within � relative � ����� ��������� ����������, ������� ���������� ������������������ 
�����������. ������� ��� � ��������� ���������� �����������������.
����������� ������� � ����� -- ������ ������� ������� � ���� �����. � ����� ����� �����, 
��������� �������� ������� � ������ ����� � � ������ ����� ���������� � �������� ������ 
������ ����� ����� ����� �������. ��������������, ���, ���� ��� ����� ��������� 
���������� ������, �� ������ ������� ����� ���� �� ����� ������� ����������. 
��� ���� �����������:

> easydiff f x h = (f (x + h) - f x) / h

����� �������� ������� �����������, �������� h ������ ���� ����� ���������. � ���������, 
���� h ������� ���������, �� ��� �������� f (x + h) � f (x) -- ����� ������ ���� �����, 
��� ��� ������ ���������� ��� ��������� ����� ��������� ���������. ��� ��������� 
������� �������� h? ���� ������� ���� ������� -- ��������� ������������������ ����������� 
�� ��� �������� � �������� ���������� h, ������� � ������� ��������. ����� 
������������������ ������ ��������� � �������� �����������, �� ������ ���������� �������� 
� �������� ����� ��-�� ������ ����������.
���� (within eps) ������������, ����� ������� ������ �����������, ������� ���������� �����, 
�� ���� ������ ����������, �������������� �� ��������� ����� ���������. �� ��������� � �������, 
����������� ������������������:

> differentiate h0 f x = map (easydiff f x) (iterate halve h0)
> halve x = x / 2

��� h0 -- ��������� �������� h, ���������������� �������� �������� ��������� �������� �� ���. 
� ������� ���� �������, ����������� � ����� ����� ����� ���� ��������� ���:

within eps (differentiate h0 f x)

�� ��� ������� �� ������ �����������������, ������ ��� ������������������ ����������� 
�������� �������� ��������. ������� ������� ���������� ����� ��� ������. �������� 
������������������ ����� ���� �������� ��� ���������� ����� + �����������, ����������� � h.
����� �������� ������������, ��� ���� ����������� ����� �������������� ������� h. 
����� A -- ���������� ����� � B * hn -- ������. ��� ��� ������ ����������� ����������� 
���������, ��������� �������� h ����� ������� ����, ��� ���� ������������ ��� �����������, 
����� ��� ���������������� ����������� ����� ���� �������� ��� 
ai = A + B * hn � ai + 1 = A + B * hn / 2n. ������ ���� ����������� ����� ���� �����θ�:
	A = (ai + 1 * 2n - ai ) / 2n - 1	(4.2)
�������, ������ ������������ �������� h ������ ��� ������ �����������. ������� ���� 
���������� ����� ��������������, �� �Ӹ �����, ��� ������� ������ �����������. �������� 
������������������ ����� ����������� �� ���� ���������������� ����� �����������, 
� ������� �������

> elimerror n (a : b : rest) =
>                        (b * 2 ^ n - a) / (2 ^ n - 1) : elimerror n (b : rest)

������� ��������� ����������� �� ������������������ ����������� � ������ ������ 
������������������, ���������� ������� �������.
�� ���� �������� ��������. ������, ��� �� ����� ������������ elimerror -- �� ������ ����� 
�������� n. ��� ������ �����������, �� ����� ��������. �������� ��������, ��� ��������� 
������ ���������, �� �� ����� ��������� ��������������.
	n = round log2 (ai + 2 - ai) / (aii + 1 - ai) - 1	(4.3)

order a : b : c : rest = round log2 (a - c) / (b - c) - 1

������ ����� ���������� ����� �������, ���������� ������������������ �����������:

> improve s = elimerror (order s) s

����������� ������� f ����� ��������� ����� ����������, ��������� improve ��������� �������:
within eps (improve (differentiate h0 f x))
������� ��� improve �������� ������ ��� ������������������� �����������, ������� ��������� 
� �������������� ��������� h, �������� �� ��� ��� ������ ����. ������, ���� ��� ��������� 
� ����� ������������������, �� ��������� -- ����� �� ������������������! ��� ��������, 
��� ������������������ ����������� ����� ���� �������� ������ ��� �������. ������ ��� 
����������� �����������, � ������������������ �������� �Ӹ ������� � �������. ����������� 
����� ��������� ����� ���������� ���������:
within eps (improve (improve (improve (differentiate h0 f x))))
� �������� �������������� ���������� ���, ��������, ����� ������� ���׸����� �������, � ���� 
������ ��������� ����� ������. ����� ���� ����������:

> super s = map second (iterate improve s)
> second (a : b : rest) = b

����� super ���������� (iterate improve), ����� �������� ������������������ �Ӹ ����� ���������� 
������������������� �����������, � ������ ����� ������������������ �����������, ���� ������ 
����������� ������ �� ���������� ������������������� (�����������, ��� ����� ����� ����� 
������ -- ��� ����� ����� ��� ������ � �� ������� �������������� ����������). ���� �������� 
������������� ����� ������� -- �� ���������� ��� ������ � ������ ��������� �����, �� ���� 
���� ��� ��� ������ ����������� ���������. ����������� ����� ����� ���������� ��������� ����������:

within eps (super (differentiate h0 f x))

��������, ��� ������ �� ������������� �������, ����� ��������� ����, �� ����� � ���, ��� ���� 
����� ������� �������� ��� super ����� ���������� ����� ������������� � �������������� ������� 
����������.

================================================================================
 
