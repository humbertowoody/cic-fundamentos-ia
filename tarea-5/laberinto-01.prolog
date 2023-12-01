%=======================================================================
%  Laberinto-01 para TAREA #5
%    a resolver con búsqueda informada...
%        UniformCost, Greedy, A*
%  Dr.Salvador Godoy C.
%        Última modificación: José Daniel Pérez Ramírez
%                             mayo 2023
%=======================================================================

:- dynamic(estado_inicial/1).
:- dynamic(estado_meta/1).

estado_inicial([15,1]).
estado_meta([15,30]).
%estado_meta([18,3]).

acceso([1,1], [[1,2],[2, 1]]).
acceso([1,2], [[1,1],[1,3]]).
acceso([1,3], [[1,2],[1,4]]).
acceso([1,4], [[1,3],[1,5]]).
acceso([1,5], [[1,4],[2,5]]).
%acceso([1,6],[[],[]]).
%acceso([1,7],[[],[]]).
%acceso([1,8],[[],[]]).
%acceso([1,9],[[],[]]).
%acceso([1,10],[[],[]]).
acceso([1,11],[[1,12],[2,11]]).
acceso([1,12],[[1,11],[1,13]]).
acceso([1,13],[[1,12],[2,13]]).
acceso([1,14],[[1,15]]).
acceso([1,15],[[1,14],[1,16]]).
acceso([1,16],[[1,15],[2,16],[1,17]]).
acceso([1,17],[[1,16],[2,17]]).
acceso([1,18],[[1,19],[2,18]]).
acceso([1,19],[[1,18],[1,20]]).
acceso([1,20],[[1,19],[2,20]]).
acceso([1,21],[[2,21],[1,22]]).
acceso([1,22],[[1,21],[1,23]]).
acceso([1,23],[[1,22],[1,24]]).
acceso([1,24],[[1,23],[1,25]]).
acceso([1,25],[[1,24],[1,26]]).
acceso([1,26],[[1,25],[1,27]]).
acceso([1,27],[[1,26],[1,28]]).
acceso([1,28],[[1,27],[2,28]]).
acceso([1,29],[[1,30],[2,29]]).
acceso([1,30],[[1,29],[2,30]]).

acceso([2,1], [[1,1],[2,2]]).
acceso([2,2], [[2,1],[2,3]]).
acceso([2,3], [[2,2],[2,4]]).
acceso([2,4], [[2,3]]).
acceso([2,5], [[1,5],[3,5]]).
%acceso([2,6],[[],[]]).
%acceso([2,7],[[],[]]).
%acceso([2,8],[[],[]]).
%acceso([2,9],[[],[]]).
%acceso([2,10],[[],[]]).
acceso([2,11],[[1,11],[3,11]]).
acceso([2,12],[[3,12]]).
acceso([2,13],[[1,13],[2,14]]).
acceso([2,14],[[2,13],[2,15]]).
acceso([2,15],[[2,14],[3,15]]).
acceso([2,16],[[1,16],[3,16]]).
acceso([2,17],[[1,17],[2,18]]).
acceso([2,18],[[1,18],[2,17]]).
acceso([2,19],[[3,19]]).
acceso([2,20],[[1,20],[2,21],[3,20]]).
acceso([2,21],[[1,21],[2,20]]).
acceso([2,22],[[2,23],[3,22]]).
acceso([2,23],[[2,22],[3,23],[2,24]]).
acceso([2,24],[[2,23]]).
acceso([2,25],[[2,26],[3,25]]).
acceso([2,26],[[2,25],[2,27]]).
acceso([2,27],[[2,26],[3,27]]).
acceso([2,28],[[1,28],[2,29]]).
acceso([2,29],[[1,29],[2,28]]).
acceso([2,30],[[1,30],[3,30]]).

acceso([3,1], [[4,1]]).
acceso([3,2], [[3,3],[4,2]]).
acceso([3,3], [[3,2],[3,4]]).
acceso([3,4], [[3,3],[3,5]]).
acceso([3,5], [[2,5],[3,4],[4,5]]).
%acceso([3,6],[[],[]]).
%acceso([3,7],[[],[]]).
%acceso([3,8],[[],[]]).
%acceso([3,9],[[],[]]).
%acceso([3,10],[[],[]]).
acceso([3,11],[[2,11],[4,11]]).
acceso([3,12],[[2,12],[3,13]]).
acceso([3,13],[[3,12],[3,14]]).
acceso([3,14],[[3,13],[3,15]]).
acceso([3,15],[[3,14],[2,15],[3,16]]).
acceso([3,16],[[2,16],[3,15]]).
acceso([3,17],[[4,17]]).
acceso([3,18],[[3,19],[4,18]]).
acceso([3,19],[[2,19],[3,18],[4,19]]).
acceso([3,20],[[2,20],[4,20]]).
%acceso([3,21],[[],[]]).
acceso([3,22],[[2,22],[4,22]]).
acceso([3,23],[[2,23],[3,24]]).
acceso([3,24],[[3,23],[3,25]]).
acceso([3,25],[[2,25],[3,24]]).
acceso([3,26],[[3,27],[4,26]]).
acceso([3,27],[[2,27],[3,26],[4,27]]).
acceso([3,28],[[4,28]]).
acceso([3,29],[[3,30],[4,29]]).
acceso([3,30],[[2,30],[3,29]]).

acceso([4,1], [[3,1],[5,1]]).
acceso([4,2], [[3,2],[4,3]]).
acceso([4,3], [[4,2],[5,3]]).
acceso([4,4], [[3,4],[4,5]]).
acceso([4,5], [[4,4],[3,5]]).
%acceso([4,6],[[],[]]).
%acceso([4,7],[[],[]]).
%acceso([4,8],[[],[]]).
%acceso([4,9],[[],[]]).
%acceso([4,10],[[],[]]).
acceso([4,11],[[3,11],[5,11]]).
acceso([4,12],[[4,13],[5,12]]).
acceso([4,13],[[4,12],[5,13]]).
acceso([4,14],[[4,15],[5,14]]).
acceso([4,15],[[4,14],[4,16]]).
acceso([4,16],[[4,15],[5,16]]).
acceso([4,17],[[3,17],[4,18]]).
acceso([4,18],[[3,18],[4,17],[5,18]]).
acceso([4,19],[[3,19],[5,19]]).
acceso([4,20],[[3,20],[5,20]]).
%acceso([4,21],[[],[]]).
acceso([4,22],[[3,22],[4,23]]).
acceso([4,23],[[4,22]]).
acceso([4,24],[[4,25],[5,24]]).
acceso([4,25],[[4,24],[4,26]]).
acceso([4,26],[[3,26],[4,25]]).
acceso([4,27],[[3,27],[4,28]]).
acceso([4,28],[[3,28],[5,28]]).
acceso([4,29],[[3,29],[4,30]]).
acceso([4,30],[[4,29],[5,30]]).


acceso([5,1], [[4,1],[5,2]]).
acceso([5,2], [[5,1],[5,3]]).
acceso([5,3], [[4,3],[5,2]]).
acceso([5,4], [[4,4],[5,5]]).
acceso([5,5], [[5,4],[6,5]]).
%acceso([5,6],[[],[]]).
%acceso([5,7],[[],[]]).
%acceso([5,8],[[],[]]).
%acceso([5,9],[[],[]]).
%acceso([5,10],[[],[]]).
acceso([5,11],[[4,11],[5,12],[6,11]]).
acceso([5,12],[[4,12],[5,11]]).
acceso([5,13],[[4,13],[5,14]]).
acceso([5,14],[[4,14],[5,13]]).
acceso([5,15],[[6,16]]).
acceso([5,16],[[4,16],[5,17]]).
acceso([5,17],[[5,16],[6,17]]).
acceso([5,18],[[4,18],[6,18]]).
acceso([5,19],[[4,19],[6,19]]).
acceso([5,20],[[4,20],[6,20]]).
%acceso([5,21],[[],[]]).
%acceso([5,22],[[],[]]).
acceso([5,23],[[5,24],[6,23]]).
acceso([5,24],[[4,24],[5,23],[6,24]]).
acceso([5,25],[[5,26],[6,25]]).
acceso([5,26],[[5,25],[5,27]]).
acceso([5,27],[[5,26],[6,27]]).
acceso([5,28],[[4,28]]).
acceso([5,29],[[5,30],[6,29]]).
acceso([5,30],[[4,30],[5,29]]).

acceso([6,1], [[7,1]]).
acceso([6,2], [[6,1],[6,3]]).
acceso([6,3], [[6,2],[6,4]]).
acceso([6,4], [[6,3],[6,5]]).
acceso([6,5], [[5,5],[6,4]]).
%acceso([6,6],[[],[]]).
%acceso([6,7],[[],[]]).
%acceso([6,8],[[],[]]).
%acceso([6,9],[[],[]]).
%acceso([6,10],[[],[]]).
acceso([6,11],[[5,11],[7,11]]).
acceso([6,12],[[6,13],[7,12]]).
acceso([6,13],[[6,12],[6,14]]).
acceso([6,14],[[6,13],[7,14],[6,15]]).
acceso([6,15],[[5,15],[6,14]]).
acceso([6,16],[[6,17],[7,16]]).
acceso([6,17],[[5,17],[6,16]]).
acceso([6,18],[[5,18],[7,18]]).
acceso([6,19],[[5,19],[6,20]]).
acceso([6,20],[[5,20],[6,19]]).
%acceso([6,21],[[],[]]).
%acceso([6,22],[[],[]]).
acceso([6,23],[[5,23],[7,23]]).
acceso([6,24],[[5,24]]).
acceso([6,25],[[5,25],[7,25]]).
acceso([6,26],[[7,25],[6,26],[7,27]]).
acceso([6,27],[[5,27],[6,28]]).
acceso([6,28],[[6,27],[6,29]]).
acceso([6,29],[[5,29],[6,28]]).
acceso([6,30],[[7,30]]).

acceso([7,1], [[6,1],[7,2]]).
acceso([7,2], [[7,1],[8,2]]).
acceso([7,3], [[7,4]]).
acceso([7,4], [[7,3],[7,5]]).
acceso([7,5], [[7,4],[8,5]]).
%acceso([7,6],[[],[]]).
%acceso([7,7],[[],[]]).
%acceso([7,8],[[],[]]).
%acceso([7,9],[[],[]]).
%acceso([7,10],[[],[]]).
acceso([7,11], [[6,11],[8,11]]).
acceso([7,12], [[6,12],[8,12]]).
acceso([7,13], [[7,14],[8,13]]).
acceso([7,14], [[6,14],[7,13]]).
acceso([7,15], [[7,16],[8,15]]).
acceso([7,16], [[6,16],[7,15]]).
acceso([7,17], [[7,18]]).
acceso([7,18], [[6,18],[8,18]]).
acceso([7,19], [[7,20]]).
acceso([7,20], [[7,19],[8,20]]).
%acceso([7,21], [[],[]]).
%acceso([7,22], [[],[]]).
acceso([7,23], [[6,23],[7,24]]).
acceso([7,24], [[7,23],[8,24]]).
acceso([7,25], [[6,25],[7,26]]).
acceso([7,26], [[7,25],[6,26],[7,27]]).
acceso([7,27], [[7,26],[7,28]]).
acceso([7,28], [[7,27],[7,29]]).
acceso([7,29], [[7,28],[7,30]]).
%acceso([7,30], [[6,30],[8,30]]). %Error
acceso([7,30], [[6,30],[8,30],[7,29]]).

acceso([8,1], [[8,2],[9,1]]).
acceso([8,2], [[7,2],[8,1]]).
acceso([8,3], [[8,4],[9,3]]).
acceso([8,4], [[8,3],[8,5]]).
acceso([8,5], [[7,5],[8,4]]).
%acceso([8,6],[[],[]]).
%acceso([8,7],[[],[]]).
%acceso([8,8],[[],[]]).
%acceso([8,9],[[],[]]).
%acceso([8,10],[[],[]]).
acceso([8,11], [[7,11],[8,12],[9,11]]).
acceso([8,12], [[7,12],[8,11]]).
acceso([8,13], [[7,13],[8,14]]).
acceso([8,14], [[8,13]]).
acceso([8,15], [[7,15],[9,15]]).
acceso([8,16], [[8,17],[9,16]]).
acceso([8,17], [[8,16],[9,17]]).
acceso([8,18], [[7,18],[9,18]]).
acceso([8,19], [[8,20],[9,19]]).
acceso([8,20], [[7,20],[8,19]]).
%acceso([8,21], [[],[]]).
%acceso([8,22], [[],[]]).
%acceso([8,23], [[],[]]).
acceso([8,24], [[7,24],[8,25],[9,24]]).
acceso([8,25], [[8,24],[8,26]]).
acceso([8,26], [[8,25],[8,27]]).
acceso([8,27], [[8,26],[8,28]]).
acceso([8,28], [[8,27],[8,29],[9,28]]).
acceso([8,29], [[8,28]]).
acceso([8,30], [[7,30],[9,30]]).

acceso([9,1], [[8,1],[9,2]]).
acceso([9,2], [[9,1],[10,2]]).
acceso([9,3], [[8,3],[10,3]]).
acceso([9,4], [[9,3],[10,4]]).
acceso([9,5], [[8,5],[10,5]]).
%acceso([9,6],[[],[]]).
%acceso([9,7],[[],[]]).
%acceso([9,8],[[],[]]).
%acceso([9,9],[[],[]]).
%acceso([9,10],[[],[]]).
acceso([9,11], [[8,11],[10,11]]).
acceso([9,12], [[9,13],[10,12]]).
acceso([9,13], [[9,12],[9,14]]).
acceso([9,14], [[9,13],[10,14]]).
acceso([9,15], [[8,15],[9,16],[10,15]]).
acceso([9,16], [[8,16],[9,15]]).
acceso([9,17], [[8,17],[10,17]]).
acceso([9,18], [[8,18],[10,18]]).
acceso([9,19], [[8,19],[9,20]]).
acceso([9,20], [[9,19],[10,20]]).
%acceso([9,21], [[],[]]).
%acceso([9,22], [[],[]]).
%acceso([9,23], [[],[]]).
acceso([9,24], [[8,24],[9,25],[10,24]]).
acceso([9,25], [[9,24],[9,26]]).
acceso([9,26], [[9,25],[10,26]]).
acceso([9,27], [[9,28],[10,27]]).
acceso([9,28], [[8,28],[9,27]]).
acceso([9,29], [[9,30],[10,29]]).
acceso([9,30], [[8,30],[9,29]]).

acceso([10,1], [[11,1]]).
acceso([10,2], [[9,2],[11,2]]).
acceso([10,3], [[9,3],[11,3]]).
acceso([10,4], [[9,4],[11,4]]).
acceso([10,5], [[9,5],[11,5]]).
%acceso([10,6],[[],[]]).
%acceso([10,7],[[],[]]).
%acceso([10,8],[[],[]]).
%acceso([10,9],[[],[]]).
%acceso([10,10],[[],[]]).
acceso([10,11], [[9,11],[11,11]]).
acceso([10,12], [[9,12],[11,12]]).
acceso([10,13], [[10,14],[11,13]]).
acceso([10,14], [[9,14],[10,13]]).
acceso([10,15], [[9,15],[10,16]]).
acceso([10,16], [[10,15]]).
acceso([10,17], [[9,17],[11,17]]).
acceso([10,18], [[9,18],[10,19]]).
acceso([10,19], [[10,18],[10,20]]).
acceso([10,20], [[9,20],[10,19],[11,20]]).
%acceso([10,21], [[],[]]).
%acceso([10,22], [[],[]]).
%acceso([10,23], [[],[]]).
acceso([10,24], [[9,24]]).
acceso([10,25], [[10,26],[11,25]]).
acceso([10,26], [[9,26],[10,25]]).
acceso([10,27], [[9,27],[11,27]]).
acceso([10,28], [[11,28]]).
acceso([10,29], [[9,29],[10,30]]).
acceso([10,30], [[10,29],[11,30]]).

acceso([11,1], [[10,1],[11,2],[12,1]]).
acceso([11,2], [[10,2],[11,1]]).
acceso([11,3], [[10,3],[11,3]]).
acceso([11,4], [[10,4],[12,4]]).
acceso([11,5], [[10,5],[12,5]]).
%acceso([11,6],[[],[]]).
%acceso([11,7],[[],[]]).
%acceso([11,8],[[],[]]).
%acceso([11,9],[[],[]]).
%acceso([11,10],[[],[]]).
acceso([11,11], [[10,11],[12,11]]).
acceso([11,12], [[10,12],[12,12]]).
acceso([11,13], [[10,13],[11,14]]).
acceso([11,14], [[11,13],[12,14]]).
acceso([11,15], [[11,16],[12,15]]).
acceso([11,16], [[11,15],[12,16]]).
acceso([11,17], [[10,17],[12,17]]).
acceso([11,18], [[11,19],[12,18]]).
acceso([11,19], [[11,18]]).
acceso([11,20], [[10,20],[12,20]]).
%acceso([11,21], [[],[]]).
%acceso([11,22], [[],[]]).
%acceso([11,23], [[],[]]).
%acceso([11,24], [[],[]]).
acceso([11,25], [[10,25],[12,25]]).
acceso([11,26], [[12,26]]).
acceso([11,27], [[10,27],[11,28]]).
acceso([11,28], [[10,28],[11,27]]).
acceso([11,29], [[11,30],[12,29]]).
acceso([11,30], [[10,30],[11,29]]).

acceso([12,1], [[11,1],[12,2]]).
acceso([12,2], [[12,1],[13,2]]).
acceso([12,3], [[11,3],[13,3]]).
acceso([12,4], [[11,4],[13,4]]).
acceso([12,5], [[11,5]]).
%acceso([12,6],[[],[]]).
%acceso([12,7],[[],[]]).
%acceso([12,8],[[],[]]).
%acceso([12,9],[[],[]]).
%acceso([12,10],[[],[]]).
acceso([12,11], [[11,11]]).
acceso([12,12], [[11,12],[12,13]]).
acceso([12,13], [[12,12],[13,13]]).
acceso([12,14], [[11,14],[12,15]]).
acceso([12,15], [[11,15],[12,14]]).
acceso([12,16], [[11,16],[12,17]]).
acceso([12,17], [[11,17],[12,16]]).
acceso([12,18], [[11,18],[12,19]]).
acceso([12,19], [[12,18],[13,19]]).
acceso([12,20], [[11,20],[13,20]]).
%acceso([12,21], [[],[]]).
%acceso([12,22], [[],[]]).
%acceso([12,23], [[],[]]).
%acceso([12,24], [[],[]]).
acceso([12,25], [[11,25],[13,25]]).
acceso([12,26], [[11,26],[12,27]]).
acceso([12,27], [[12,26],[12,28]]).
acceso([12,28], [[11,28],[12,27]]).
acceso([12,29], [[11,29],[13,29]]).
acceso([12,30], [[13,30]]).

acceso([13,1], [[13,2],[14,1]]).
acceso([13,2], [[12,2],[13,1]]).
acceso([13,3], [[12,3]]).
acceso([13,4], [[12,4],[13,5]]).
acceso([13,5], [[13,4],[14,5]]).
%acceso([13,6],[[],[]]).
%acceso([13,7],[[],[]]).
%acceso([13,8],[[],[]]).
%acceso([13,9],[[],[]]).
%acceso([13,10],[[],[]]).
acceso([13,11], [[13,12],[14,11]]).
acceso([13,12], [[13,11],[13,13]]).
acceso([13,13], [[12,13],[13,12]]).
acceso([13,14], [[13,15],[14,14]]).
acceso([13,15], [[13,14],[14,15]]).
acceso([13,16], [[13,17]]).
acceso([13,17], [[13,16],[13,18],[14,17]]).
acceso([13,18], [[13,17],[13,19]]).
acceso([13,19], [[12,19],[13,18]]).
acceso([13,20], [[12,20],[14,20]]).
%acceso([13,21], [[],[]]).
%acceso([13,22], [[],[]]).
%acceso([13,23], [[],[]]).
%acceso([13,24], [[],[]]).
acceso([13,25], [[12,25],[13,26]]).
acceso([13,26], [[13,25],[14,26]]).
acceso([13,27], [[13,28],[14,27]]).
acceso([13,28], [[13,27],[14,28]]).
acceso([13,29], [[12,29],[13,30]]).
acceso([13,30], [[12,30],[13,29],[14,30]]).

acceso([14,1], [[13,1],[14,2]]).
acceso([14,2], [[14,1],[14,3]]).
acceso([14,3], [[14,2],[14,4]]).
acceso([14,4], [[14,3],[14,5]]).
acceso([14,5], [[13,5],[14,4],[15,5]]).
%acceso([14,6],[[],[]]).
%acceso([14,7],[[],[]]).
%acceso([14,8],[[],[]]).
%acceso([14,9],[[],[]]).
%acceso([14,10],[[],[]]).
acceso([14,11], [[13,11],[15,11]]).
acceso([14,12], [[14,13],[15,12]]).
acceso([14,13], [[14,12],[14,14]]).
acceso([14,14], [[13,14],[14,13]]).
acceso([14,15], [[13,15],[14,16]]).
%acceso([14,16], [[14,15],[14,16]]). %Error
acceso([14,16], [[14,15],[15,16]]).
acceso([14,17], [[13,17],[14,18],[15,17]]).
acceso([14,18], [[14,17],[14,19]]).
acceso([14,19], [[14,18],[14,20]]).
acceso([14,20], [[13,20],[14,19]]).
%acceso([14,21], [[],[]]).
%acceso([14,22], [[],[]]).
%acceso([14,23], [[],[]]).
%acceso([14,24], [[],[]]).
%acceso([14,25], [[],[]]).
acceso([14,26], [[13,26],[14,27],[15,26]]).
acceso([14,27], [[13,27],[14,26]]).
acceso([14,28], [[13,28],[14,29]]).
acceso([14,29], [[14,28],[14,30]]).
acceso([14,30], [[13,30],[14,29]]).

acceso([15,1], [[15,2],[16,1]]).
acceso([15,2], [[15,1],[16,2]]).
acceso([15,3], [[15,4],[16,3]]).
acceso([15,4], [[15,3],[16,4]]).
acceso([15,5], [[14,5],[16,5]]).
acceso([15,6], [[16,6]]).
%acceso([15,7],[[],[]]).
%acceso([15,8],[[],[]]).
%acceso([15,9],[[],[]]).
%acceso([15,10],[[],[]]).
acceso([15,11], [[14,11],[15,12]]).
acceso([15,12], [[14,12],[15,11],[15,13]]).
acceso([15,13], [[15,12]]).
acceso([15,14], [[15,15],[16,14]]).
acceso([15,15], [[15,14],[16,15]]).
acceso([15,16], [[14,16],[16,16]]).
acceso([15,17], [[14,17],[15,18]]).
acceso([15,18], [[15,17]]).
acceso([15,19], [[15,20],[16,19]]).
acceso([15,20], [[15,19],[16,20]]).
%acceso([15,21], [[],[]]).
%acceso([15,22], [[],[]]).
%acceso([15,23], [[],[]]).
%acceso([15,24], [[],[]]).
%acceso([15,25], [[],[]]).
acceso([15,26], [[14,26],[16,26]]).
acceso([15,27], [[15,28],[16,27]]).
acceso([15,28], [[15,27],[15,29]]).
acceso([15,29], [[15,28],[15,30]]).
acceso([15,30], [[15,29],[16,30]]).

acceso([16,1], [[15,1],[17,1]]).
acceso([16,2], [[15,2],[17,2]]).
acceso([16,3], [[15,3],[17,3]]).
acceso([16,4], [[15,4],[16,5]]).
acceso([16,5], [[15,5],[16,4]]).
acceso([16,6], [[15,6],[17,6]]).
%acceso([16,7],[[],[]]).
%acceso([16,8],[[],[]]).
%acceso([16,9],[[],[]]).
%acceso([16,10],[[],[]]).
acceso([16,11], [[16,12],[17,11]]).
acceso([16,12], [[16,11],[16,13]]).
acceso([16,13], [[16,12],[16,14]]).
acceso([16,14], [[16,13],[15,14]]).
acceso([16,15], [[15,15],[17,15]]).
%acceso([16,16], [[15,16],[17,16]]). %Error
acceso([16,16], [[15,16],[17,16],[16,17]]).
acceso([16,17], [[16,16],[16,18]]).
acceso([16,18], [[16,17],[16,19]]).
acceso([16,19], [[16,18],[15,19]]).
acceso([16,20], [[15,20],[17,20]]).
%acceso([16,21],[[],[]]).
%acceso([16,22],[[],[]]).
%acceso([16,23],[[],[]]).
%acceso([16,24],[[],[]]).
%acceso([16,25],[[],[]]).
acceso([16,26],[[15,26],[17,26]]).
acceso([16,27],[[15,27],[16,28]]).
acceso([16,28],[[16,27],[16,29]]).
acceso([16,29],[[16,28]]).
acceso([16,30],[[15,30],[17,30]]).

acceso([17,1], [[16,1],[18,1]]).
acceso([17,2], [[16,2],[18,2]]).
acceso([17,3], [[16,3],[17,4]]).
acceso([17,4], [[17,3],[18,4]]).
acceso([17,5], [[17,6],[18,5]]).
acceso([17,6], [[16,6],[18,6]]).
%acceso([17,7],[[],[]]).
%acceso([17,8],[[],[]]).
%acceso([17,9],[[],[]]).
%acceso([17,10],[[],[]]).
acceso([17,11],[[16,11],[18,11]]).
acceso([17,12],[[17,13],[18,12]]).
acceso([17,13],[[17,12]]).
acceso([17,14],[[17,15],[18,14]]).
acceso([17,15],[[16,15],[17,14]]).
acceso([17,16],[[16,16],[18,16]]).
acceso([17,17],[[17,18],[18,17]]).
acceso([17,18],[[17,17],[17,19]]).
acceso([17,19],[[17,18]]).
acceso([17,20],[[16,20],[18,20]]).
%acceso([17,21],[[],[]]).
%acceso([17,22],[[],[]]).
%acceso([17,23],[[],[]]).
%acceso([17,24],[[],[]]).
%acceso([17,25],[[],[]]).
acceso([17,26],[[16,26],[18,26]]).
acceso([17,27],[[17,28]]).
acceso([17,28],[[17,27],[16,28]]).
acceso([17,29],[[17,30],[18,29]]).
acceso([17,30],[[16,30],[17,29],[18,30]]).

acceso([18,1], [[17,1],[19,1]]).
acceso([18,2], [[17,2],[18,3]]).
acceso([18,3], [[18,2]]).
acceso([18,4], [[17,4],[18,5]]).
acceso([18,5], [[17,5],[18,4]]).
acceso([18,6], [[17,6],[18,7]]).
acceso([18,7], [[18,6],[19,7]]).
%acceso([18,8],[[],[]]).
%acceso([18,9],[[],[]]).
%acceso([18,10],[[],[]]).
acceso([18,11],[[17,11],[19,11]]).
%acceso([18,12],[[17,12],[19,12]]).
acceso([18,12],[[17,12],[19,12],[18,13]]).
acceso([18,13],[[18,12],[18,14]]).
acceso([18,14],[[17,14],[18,13]]).
acceso([18,15],[[18,16],[19,15]]).
acceso([18,16],[[17,16],[18,15]]).
acceso([18,17],[[17,17]]).
acceso([18,18],[[17,18],[19,18]]).
acceso([18,19],[[18,20],[19,19]]).
acceso([18,20],[[17,20],[18,19],[19,20]]).
%acceso([18,21],[[],[]]).
%acceso([18,22],[[],[]]).
%acceso([18,23],[[],[]]).
%acceso([18,24],[[],[]]).
%acceso([18,25],[[],[]]).
acceso([18,26],[[17,26],[19,26]]).
acceso([18,27],[[19,27],[18,28]]).
acceso([18,28],[[18,27],[19,28]]).
acceso([18,29],[[17,29],[19,29]]).
acceso([18,30],[[17,30]]).

acceso([19,1], [[18,1],[20,1]]).
acceso([19,2], [[19,3],[20,2]]).
acceso([19,3], [[19,2],[20,3],[19,4]]).
acceso([19,4], [[19,3],[20,4],[19,5]]).
acceso([19,5], [[19,4],[19,6]]).
acceso([19,6], [[19,5]]).
acceso([19,7], [[18,7],[20,7]]).
%acceso([19,8],[[],[]]).
%acceso([19,9],[[],[]]).
%acceso([19,10],[[],[]]).
acceso([19,11],[[18,11],[20,11]]).
acceso([19,12],[[18,12],[20,12]]).
acceso([19,13],[[19,14]]).
acceso([19,14],[[19,13],[20,14]]).
acceso([19,15],[[18,15],[20,15]]).
acceso([19,16],[[19,17],[20,16]]).
acceso([19,17],[[19,16],[19,18]]).
acceso([19,18],[[18,18],[19,17]]).
acceso([19,19],[[18,19],[20,19]]).
acceso([19,20],[[18,20]]).
%acceso([19,21],[[],[]]).
%acceso([19,22],[[],[]]).
%acceso([19,23],[[],[]]).
%acceso([19,24],[[],[]]).
%acceso([19,25],[[],[]]).
acceso([19,26],[[18,26],[19,27]]).
acceso([19,27],[[19,26],[18,27]]).
acceso([19,28],[[18,28],[20,28]]).
acceso([19,29],[[18,29],[19,30]]).
acceso([19,30],[[19,29],[20,30]]).

acceso([20,1], [[19,1],[21,1]]).
acceso([20,2], [[19,2],[21,2]]).
acceso([20,3], [[19,3],[21,3]]).
acceso([20,4], [[19,4]]).
acceso([20,5], [[20,6],[21,5]]).
acceso([20,6], [[20,5],[20,7]]).
acceso([20,7], [[19,7],[20,6]]).
%acceso([20,8],[[],[]]).
%acceso([20,9],[[],[]]).
%acceso([20,10],[[],[]]).
acceso([20,11],[[19,11],[21,11]]).
acceso([20,12],[[19,12],[20,13]]).
acceso([20,13],[[20,12],[21,13]]).
acceso([20,14],[[21,13],[22,14]]).
acceso([20,15],[[20,14],[20,16]]).
acceso([20,16],[[19,16],[20,15],[21,16]]).
acceso([20,17],[[20,18],[21,17]]).
acceso([20,18],[[20,17],[21,18]]).
acceso([20,19],[[19,19],[20,20]]).
acceso([20,20],[[20,19],[21,20]]).
%acceso([20,21],[[],[]]).
%acceso([20,22],[[],[]]).
%acceso([20,23],[[],[]]).
%acceso([20,24],[[],[]]).
%acceso([20,25],[[],[]]).
acceso([20,26],[[20,27],[21,26]]).
acceso([20,27],[[20,26],[21,27]]).
acceso([20,28],[[19,28],[20,29]]).
acceso([20,29],[[20,28],[21,29]]).
acceso([20,30],[[19,30],[21,30]]).

%acceso([21,1], [[20,1],[22,1]]).%Error
acceso([21,1], [[20,1],[22,1],[21,2]]).
acceso([21,2], [[20,2],[21,1]]).
acceso([21,3], [[20,3],[21,4]]).
acceso([21,4], [[21,3],[22,4]]).
acceso([21,5], [[20,5],[22,5]]).
acceso([21,6], [[21,7],[22,6]]).
acceso([21,7], [[21,6],[21,8]]).
acceso([21,8], [[21,7],[22,8]]).
%acceso([21,9],[[],[]]).
%acceso([21,10],[[],[]]).
acceso([21,11],[[20,11],[21,12],[22,11]]).
acceso([21,12],[[21,11],[22,12]]).
acceso([21,13],[[20,13],[21,14]]).
acceso([21,14],[[21,13],[22,14]]).
acceso([21,15],[[21,16]]).
acceso([21,16],[[21,15],[20,16]]).
acceso([21,17],[[20,17],[22,17]]).
acceso([21,18],[[20,18],[22,18]]).
acceso([21,19],[[21,20],[22,19]]).
acceso([21,20],[[20,20],[21,19],[22,20]]).
%acceso([21,21],[[],[]]).
%acceso([21,22],[[],[]]).
%acceso([21,23],[[],[]]).
%acceso([21,24],[[],[]]).
%acceso([21,25],[[],[]]).
acceso([21,26],[[20,26],[22,26]]).
acceso([21,27],[[20,27],[21,28],[22,27]]).
acceso([21,28],[[21,27]]).
acceso([21,29],[[20,29],[21,30]]).
acceso([21,30],[[20,30],[21,29],[22,30]]).

acceso([22,1], [[21,1],[22,2]]).
acceso([22,2], [[22,1],[22,3]]).
acceso([22,3], [[22,2],[23,3]]).
acceso([22,4], [[21,4],[22,5]]).
acceso([22,5], [[21,5],[22,4]]).
acceso([22,6], [[21,6],[23,6]]).
acceso([22,7], [[23,7]]).
acceso([22,8], [[21,8],[23,8]]).
%acceso([22,9],[[],[]]).
%acceso([22,10],[[],[]]).
acceso([22,11],[[21,11]]).
acceso([22,12],[[21,12],[23,12]]).
acceso([22,13],[[23,13]]).
acceso([22,14],[[21,14],[22,15]]).
acceso([22,15],[[22,14],[22,16]]).
acceso([22,16],[[22,15],[23,16]]).
acceso([22,17],[[21,17],[23,17]]).
acceso([22,18],[[21,18],[22,19]]).
acceso([22,19],[[22,18],[21,19]]).
acceso([22,20],[[21,20],[23,20]]).
%acceso([22,21],[[],[]]).
%acceso([22,22],[[],[]]).
%acceso([22,23],[[],[]]).
%acceso([22,24],[[],[]]).
%acceso([22,25],[[],[]]).
acceso([22,26],[[21,26],[23,26]]).
acceso([22,27],[[21,27],[22,28]]).
acceso([22,28],[[22,27],[22,29]]).
acceso([22,29],[[22,28]]).
acceso([22,30],[[21,30],[23,30]]).

acceso([23,1], [[23,2],[24,1]]).
acceso([23,2], [[23,1]]).
acceso([23,3], [[22,3],[23,4]]).
acceso([23,4], [[23,3],[23,5]]).
acceso([23,5], [[23,4],[23,6]]).
acceso([23,6], [[22,6],[23,5]]).
acceso([23,7], [[22,7],[24,7]]).
acceso([23,8], [[22,8],[24,8]]).
%acceso([23,9],[[],[]]).
%acceso([23,10],[[],[]]).
acceso([23,11],[[23,12],[24,11]]).
acceso([23,12],[[22,12],[23,11]]).
acceso([23,13],[[22,13],[23,14],[24,13]]).
acceso([23,14],[[23,13],[23,15]]).
acceso([23,15],[[23,14],[24,15]]).
acceso([23,16],[[22,16],[23,17]]).
acceso([23,17],[[23,16],[22,17]]).
acceso([23,18],[[24,18]]).
acceso([23,19],[[23,20],[24,19]]).
acceso([23,20],[[22,20],[23,19]]).
%acceso([23,21],[[],[]]).
%acceso([23,22],[[],[]]).
%acceso([23,23],[[],[]]).
%acceso([23,24],[[],[]]).
%acceso([23,25],[[],[]]).
acceso([23,26],[[22,26],[24,26]]).
acceso([23,27],[[24,27]]).
acceso([23,28],[[22,28],[23,29]]).
acceso([23,29],[[23,28],[24,29]]).
acceso([23,30],[[22,30],[24,30]]).

acceso([24,1], [[23,1],[24,2],[25,1]]).
acceso([24,2], [[24,1],[24,3]]).
acceso([24,3], [[24,2],[24,4]]).
acceso([24,4], [[24,3],[25,4],[24,5]]).
acceso([24,5], [[24,4],[24,6]]).
acceso([24,6], [[24,5],[24,7]]).
acceso([24,7], [[23,7],[24,6],[25,7]]).
acceso([24,8], [[23,8],[24,9]]).
acceso([24,9], [[24,8],[25,9]]).
%acceso([24,10],[[],[]]).
acceso([24,11],[[23,11],[25,11]]).
acceso([24,12],[[24,13]]).
acceso([24,13],[[23,13],[24,12],[25,13]]).
acceso([24,14],[[24,15]]).
acceso([24,15],[[24,14],[23,15],[24,16]]).
acceso([24,16],[[24,15],[25,16]]).
acceso([24,17],[[25,17],[24,18]]).
acceso([24,18],[[23,18],[24,17],[25,18]]).
acceso([24,19],[[23,19],[24,20]]).
acceso([24,20],[[24,19],[25,20]]).
%acceso([24,21],[[],[]]).
%acceso([24,22],[[],[]]).
%acceso([24,23],[[],[]]).
%acceso([24,24],[[],[]]).
%acceso([24,25],[[],[]]).
acceso([24,26],[[23,26],[25,26]]).
acceso([24,27],[[23,27],[24,28]]).
acceso([24,28],[[24,27],[25,28]]).
acceso([24,29],[[23,29],[25,29]]).
acceso([24,30],[[23,30],[25,30]]).

acceso([25,1], [[24,1],[25,2]]).
acceso([25,2], [[25,1],[25,3]]).
acceso([25,3], [[25,2]]).
acceso([25,4], [[24,4],[25,5]]).
acceso([25,5], [[25,4],[26,5]]).
acceso([25,6], [[25,7]]).
acceso([25,7], [[24,7],[25,6],[25,8]]).
acceso([25,8], [[25,7],[26,8]]).
acceso([25,9], [[24,9],[26,9]]).
%acceso([19,10],[[],[]]).
acceso([25,11],[[24,11],[25,12],[26,11]]).
acceso([25,12],[[25,11],[26,12]]).
acceso([25,13],[[24,13],[25,14]]).
acceso([25,14],[[25,13],[26,14]]).
acceso([25,15],[[26,15]]).
acceso([25,16],[[24,16],[26,16]]).
acceso([25,17],[[24,18],[25,17]]).
acceso([25,18],[[23,18],[24,17],[25,18]]).
acceso([25,19],[[25,18],[26,19]]).
acceso([25,20],[[24,20],[26,20]]).
%acceso([19,21],[[],[]]).
%acceso([19,22],[[],[]]).
%acceso([19,23],[[],[]]).
%acceso([19,24],[[],[]]).
%acceso([19,25],[[],[]]).
acceso([25,26],[[24,26],[26,26]]).
acceso([25,27],[[25,28],[26,27]]).
acceso([25,28],[[25,27],[26,28]]).
acceso([25,29],[[24,29],[26,29]]).
acceso([25,30],[[24,30],[26,30]]).

acceso([26,1], [[26,2],[27,1]]).
acceso([26,2], [[25,2],[26,1]]).
acceso([26,3], [[26,4],[27,3]]).
acceso([26,4], [[26,3],[26,5]]).
acceso([26,5], [[25,5],[26,4]]).
acceso([26,6], [[26,7],[27,6]]).
acceso([26,7], [[26,6],[27,7]]).
acceso([26,8], [[25,8]]).
acceso([26,9], [[25,9],[27,9]]).
acceso([26,10], [[26,11]]).
acceso([26,11], [[25,11],[27,11]]).
acceso([26,12], [[25,12],[26,13]]).
acceso([26,13], [[26,12],[27,13]]).
acceso([26,14], [[25,14],[27,14]]).
acceso([26,15], [[25,15],[26,16]]).
acceso([26,16], [[25,16],[26,15]]).
acceso([26,17], [[25,17],[27,17]]).
acceso([26,18], [[27,18]]).
acceso([26,19], [[25,19],[26,20]]).
acceso([26,20], [[25,20],[26,19]]).
%acceso([26,21],[[],[]]).
%acceso([26,22],[[],[]]).
%acceso([26,23],[[],[]]).
%acceso([26,24],[[],[]]).
%acceso([26,25],[[],[]]).
acceso([26,26],[[25,26],[27,26]]).
acceso([26,27],[[25,27],[27,27]]).
acceso([26,28],[[25,28],[27,28]]).
acceso([26,29],[[25,29],[26,30]]).
acceso([26,30],[[26,29],[25,30]]).

acceso([27,1], [[26,1],[28,1]]).
acceso([27,2], [[27,3],[28,2]]).
acceso([27,3], [[26,3],[27,2]]).
acceso([27,4], [[27,5],[28,4]]).
acceso([27,5], [[27,4],[27,6]]).
acceso([27,6], [[26,6],[27,5]]).
acceso([27,7], [[26,7],[27,8]]).
acceso([27,8], [[27,7],[28,8]]).
acceso([27,9], [[26,9],[27,10]]).
acceso([27,10], [[27,9],[28,10]]).
acceso([27,11], [[26,11],[28,11]]).
acceso([27,12], [[28,12]]).
acceso([27,13], [[26,13],[28,13]]).
acceso([27,14], [[26,14],[28,14]]).
acceso([27,15], [[26,15],[27,16]]).
acceso([27,16], [[27,15]]).
acceso([27,17], [[26,17],[28,17]]).
acceso([27,18], [[26,18],[27,19],[28,18]]).
acceso([27,19], [[27,18],[28,19],[27,20]]).
acceso([27,20], [[27,19],[28,20]]).
%acceso([27,21],[[],[]]).
%acceso([27,22],[[],[]]).
%acceso([27,23],[[],[]]).
%acceso([27,24],[[],[]]).
%acceso([27,25],[[],[]]).
acceso([27,26], [[26,22],[28,26]]).
acceso([27,27], [[26,27],[28,27]]).
acceso([27,28], [[26,28],[28,28]]).
acceso([27,29], [[27,30],[28,29]]).
acceso([27,30], [[27,29]]).

acceso([28,1], [[27,1],[29,1]]).
acceso([28,2], [[27,2],[28,3]]).
acceso([28,3], [[28,2],[29,3]]).
acceso([28,4], [[27,4],[29,4]]).
acceso([28,5], [[29,5]]).
acceso([28,6], [[28,7],[29,6]]).
acceso([28,7], [[28,6],[29,7]]).
acceso([28,8], [[27,8],[28,9]]).
acceso([28,9], [[28,8],[28,10]]).
acceso([28,10], [[27,10],[28,9]]).
acceso([28,11], [[27,11],[29,11]]).
acceso([28,12], [[27,12],[28,11]]).
acceso([28,13], [[27,13],[29,13]]).
acceso([28,14], [[27,14],[28,15],[29,14]]).
acceso([28,15], [[28,14],[28,16]]).
acceso([28,16], [[28,15],[28,17]]).
acceso([28,17], [[27,17],[28,16]]).
acceso([28,18], [[27,18]]).
acceso([28,19], [[27,19],[29,19]]).
acceso([28,20], [[27,20],[29,20]]).
%acceso([28,21],[[],[]]).
%acceso([28,22],[[],[]]).
%acceso([28,23],[[],[]]).
%acceso([28,24],[[],[]]).
%acceso([28,25],[[],[]]).
acceso([28,26], [[27,26],[28,27]]).
acceso([28,27], [[27,27],[28,26]]).
acceso([28,28], [[27,28],[29,28]]).
acceso([28,29], [[27,29],[28,30]]).
acceso([28,30], [[28,29],[29,30]]).

acceso([29,1], [[28,1],[30,1]]).
acceso([29,2], [[30,2]]).
acceso([29,3], [[28,3],[30,3]]).
acceso([29,4], [[28,4],[30,4]]).
acceso([29,5], [[28,5],[29,6]]).
acceso([29,6], [[28,6],[29,5]]).
%acceso([29,7], [[28,7],[30,7]]). %Error
acceso([29,7], [[28,7],[29,8],[30,7]]).
acceso([29,8], [[29,7],[30,8]]).
acceso([29,9], [[29,10]]).
acceso([29,10], [[29,9],[29,11]]).
acceso([29,11], [[28,11],[29,10]]).
acceso([29,12], [[29,13],[30,12]]).
acceso([29,13], [[28,13],[29,12]]).
acceso([29,14], [[28,14],[30,14]]).
acceso([29,15], [[29,16],[30,15]]).
acceso([29,16], [[29,15],[30,16]]).
acceso([29,17], [[29,18],[30,17]]).
acceso([29,18], [[29,17],[29,19]]).
acceso([29,19], [[28,19],[29,18]]).
acceso([29,20], [[28,20],[30,20]]).
%acceso([29,21],[[],[]]).
%acceso([29,22],[[],[]]).
%acceso([29,23],[[],[]]).
%acceso([29,24],[[],[]]).
%acceso([29,25],[[],[]]).
acceso([29,26], [[30,26]]).
acceso([29,27], [[29,28],[30,27]]).
acceso([29,28], [[28,28],[29,27]]).
acceso([29,29], [[29,30],[30,29]]).
acceso([29,30], [[28,30],[29,29],[30,30]]).

acceso([30,1], [[29,1],[30,2]]).
acceso([30,2], [[29,2],[30,1]]).
acceso([30,3], [[29,3],[30,4]]).
%acceso([30,4], [[29,4],[30,3]]). %Error
acceso([30,4], [[29,4],[30,3],[30,5]]).
acceso([30,5], [[30,4],[30,6]]).
acceso([30,6], [[30,5],[30,7]]).
acceso([30,7], [[29,7],[30,6]]).
acceso([30,8], [[29,8],[30,9]]).
acceso([30,9], [[30,8],[30,10]]).
acceso([30,10], [[30,9],[30,11]]).
acceso([30,11], [[30,10],[30,12]]).
acceso([30,12], [[29,12],[30,11]]).
acceso([30,13], [[30,14]]).
acceso([30,14], [[29,14],[30,13],[30,15]]).
acceso([30,15], [[29,15],[30,14]]).
acceso([30,16], [[29,16],[30,17]]).
acceso([30,17], [[29,17],[30,16]]).
acceso([30,18], [[30,19]]).
acceso([30,19], [[30,18],[30,20]]).
acceso([30,20], [[29,20],[30,19]]).
%acceso([30,21],[[],[]]).
%acceso([30,22],[[],[]]).
%acceso([30,23],[[],[]]).
%acceso([30,24],[[],[]]).
%acceso([30,25],[[],[]]).
acceso([30,26], [[29,26],[30,27]]).
acceso([30,27], [[29,27],[30,26],[30,28]]).
acceso([30,28], [[30,27],[30,29]]).
acceso([30,29], [[29,29],[30,28]]).
acceso([30,30], [[29,30]]).
