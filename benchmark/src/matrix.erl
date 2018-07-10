%%%% Author Tankerdream
%%%% https://www.jianshu.com/p/a9360e098319

-compile(export_all).

-module(matrix).

gen(M,N) ->
    row(M,N,[]).

row(M,N,Result) ->
    case 0 =:= M of
        true -> Result;
        false -> row(M-1,N,Result++[column(N,[])])
    end.

column(N,Result) ->
    case 0 =:= N of
        true -> Result;
        false -> column(N-1,Result++[rand:uniform(2)])
    end.

%% 单线程
multiple(A,B) ->
    R = lists:foldl(fun(Element,Acc) -> Acc++[rowMultiplyColumn(Element,B,[],1)] end, [], A).

%%@param 
 % A,B:list
 % Result:存放结果，初始为[]
 % Count:控制列数,初始为1
 % Result：存放结果，初始为[]   
%%将某一行A与矩阵B相乘，得出一组向量 
rowMultiplyColumn(A,B,Result,Count) ->
    case Count =:= length(lists:nth(1,B))+1 of
        true -> Result;
        false -> 
            rowMultiplyColumn(A,B,Result ++ [multiply(A,getColumn(B,Count),0)],Count+1)
    end.

%% 得到矩阵B的第Num列
getColumn(B,Num) ->
    lists:foldl(fun(A,Acc) -> Acc++[lists:nth(Num, A)] end, [],B).

%% 计算两个向量List的数量积
multiply([H1|T1],[H2|T2],Sum)  when length(T1) =:= length(T2)  ->
multiply(T1,T2,Sum+H1*H2);
multiply([],[],Sum) -> Sum.

