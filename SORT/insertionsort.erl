% The authors of this work have released all rights to it and placed it
% in the public domain under the Creative Commons CC0 1.0 waiver
% (http://creativecommons.org/publicdomain/zero/1.0/).
% 
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
% CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
% SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
% 
% Retrieved from: http://en.literateprograms.org/Insertion_sort_(Erlang)?oldid=19162

% Sort a list %

-module(isort).
-export([sort/1]).

sort(List) when is_list(List) ->
    sort(List, []).

sort([H | T], []) ->
    sort(T, [H]);
 
sort([H | T], [AccH | AccT]) when H =< AccH ->
    sort(T, [H, AccH | AccT]);

sort([H | T], [AccH | AccT]) ->
    NewAcc = [AccH | sort(H, AccT)],
    sort(T, NewAcc);

sort([], Acc) ->
    Acc;
sort(H, [AccH | AccT]) when H =< AccH ->
    [H, AccH | AccT];

sort(H, [AccH | AccT]) ->
    [AccH | sort(H, AccT)];
 
sort(H, []) ->
    [H].
    
