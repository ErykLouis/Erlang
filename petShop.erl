-module(petShop).
-define(ERROR_PET, "Pet was not found!").
-define(SUCCESFUL, "Action was succesful!").

%% avem produse unice, set table
-record(store, {item, 
                price, 
                amount}).
-record(funds, {  
                  money_in, 
                  day, 
                  currency,
                  money_out}).
-record(safe_box, {  amount,
                     day, 
                     currency 
                     }).

-export([get_safe_box_money_by_day/1, 
        get_safe_box_money/0, 
        get_money_in/0, 
        get_money_out/0, 
        install/1, 
        insert_record_in_table/1, 
        check_price_of_pet/1, 
        buy_pet/5 , 
        sell_pet/3, 
        get_money_by_day_and_currency/2,
        worker/0,
        start_worker/0,
        customer/2,
        owner/5,
        owner/3,
        accountant/2,
        accountant/3]).

install(_Nodes) ->
    %% asigura ca stanga e egala cu  dreapta
   % ok = mnesia:create_schema(Nodes),
   % application:start(mnesia),
    mnesia:create_table(table_store,
            [{attributes, record_info(fields, store)}
            ]),
    mnesia:create_table(table_funds,
            [{attributes, record_info(fields, funds)},
             {index, [#funds.day, #funds.currency]},
             {type, bag}

            ]),
     mnesia:create_table(table_safe_box,
             [{attributes, record_info(fields, safe_box)},
              {index, [#safe_box.day, #safe_box.currency]},
              {type, bag}
             ]).
     %application:stop(mnesia).

insert_record_in_table(Record) ->
    Fun = fun() -> mnesia:write(Record)
          end,
    mnesia:transaction(Fun).

check_price_of_pet(PetName) ->
        Fun = fun() ->
                case mnesia:read({table_store, PetName}) of
                [#store{price = P}] ->
                        {PetName, P};
                [] -> undefined
                end
        end,
        mnesia:transaction(Fun).

%vindem un animal
sell_pet(PetName, Currency, Day) ->
        Fun = fun() ->
                case mnesia:read({table_store, PetName}) of
                        [#store{item = _I, price = P, amount = A}] ->
                                %verificam daca avem animalul in stoc
                                case A of
                                        0 -> undefined;
                                        %daca este disponibil inseram in tabela pets un animal cu amount-1  adaugam o plata
                                        _ -> 
                                                Record = #store{ item = PetName, price = P, amount = A-1},
                                                MoneyIncome = #funds{money_in = P, day = Day, currency = Currency, money_out = 0},
                                                mnesia:write({table_store, Record}),
                                                mnesia:write({table_funds, MoneyIncome})
                                end;
                        [] -> undefined
                end
        end,
        mnesia:transaction(Fun).

% daca exista deja animalul adaugam la amount 1, daca nu exista scriem in tabela un record nou
buy_pet(PetName, SellingPrice, BuyoutPrice, Currency, Day) ->
         Fun = fun() ->
                case mnesia:read({table_store, PetName}) of
                        [#store{item = _I, price = P, amount = A}] ->
                                        Record = #store{ item = PetName, price = P, amount = A+1},
                                        mnesia:write({table_store, Record});
                        [] -> 
                                Record = #store{ item = PetName, price = SellingPrice, amount = 1},
                                mnesia:write({table_store, Record})
                end,
                MoneyIncome = #funds{money_in = 0, day = Day, currency = Currency, money_out = BuyoutPrice},       
                mnesia:write({table_funds, MoneyIncome})
        end,
        mnesia:transaction(Fun).

%list_ballance([H|T], Acc) ->


%afisam profitul dintr-o zi in functie de Currency
get_money_by_day_and_currency(Day, Currency) ->
        Fun = fun() ->
                case mnesia:read({table_funds, Day, Currency}) of
                        [] -> 0;
                        FoundList->
                                list:foldl(fun(#funds{ money_in = MI, money_out = MO}, Acc) -> Acc + (MI - MO) 
                                           end, 0, FoundList)
                end
        end,
        mnesia:transaction(Fun).

get_money_in() ->
        Fun = fun() ->
                case mnesia:read(table_funds) of
                         [] -> 0;
                        FoundList->
                                list:foldl(fun(#funds{ money_in = MI}, Acc) -> Acc + MI
                                           end, 0, FoundList)
                end
        end,
        mnesia:transaction(Fun).

get_money_out() ->
        Fun = fun() ->
                case mnesia:read(table_funds) of
                        [] -> 0;
                        FoundList->
                                list:foldl(fun(#funds{ money_out = MO}, Acc) -> Acc + MO
                                           end, 0, FoundList)
                end
        end,
        mnesia:transaction(Fun).

get_safe_box_money() ->
        Fun = fun() ->
                case mnesia:read(table_safe_box) of
                         [] -> 0;
                        FoundList->
                                list:foldl(fun(#safe_box{ amount = A}, Acc) -> Acc + A
                                           end, 0, FoundList)
                end
        end,
        mnesia:transaction(Fun).

get_safe_box_money_by_day(Day)  ->
        Fun = fun() ->
                case mnesia:read(table_safe_box) of
                        [] -> 0;
                        FoundList->
                                list:foldl(fun(#safe_box{ amount = A, day = D}, Acc) ->
                                        case D of
                                                Day -> Acc + A;
                                                _ -> Acc
                                        end
                                           end, 0, FoundList)
                end
        end,
        mnesia:transaction(Fun).


customer(WorkerPid, PetName) ->
        WorkerPid ! {self(), {"Check Price", PetName}},
        receive
                {WorkerPid, undefined} -> ?ERROR_PET;
                {WorkerPid, Result} -> Result
        after 2000 ->
                timeout
        end.

owner(PetName, SellingPrice, BuyoutPrice, Currency, Day) ->
       WorkerPid = start_worker(),
       WorkerPid ! {self(), {"Buy Pet", PetName, SellingPrice, BuyoutPrice, Currency, Day}},
       receive
               {WorkerPid, undefined} -> ?ERROR_PET;
               {WorkerPid, ok} -> ?SUCCESFUL
        after 2000 ->
                timeout
        end.
owner(PetName, Currency, Day) ->
       WorkerPid = start_worker(),
       WorkerPid ! {self(), {"Sell Pet", PetName, Currency, Day}},
       receive
               {WorkerPid, undefined} -> ?ERROR_PET;
               {WorkerPid, ok} -> ?SUCCESFUL
        after 2000 ->
                timeout
        end.
accountant(WorkerPid, Variable) when is_number(Variable) ->
        WorkerPid  ! {self(), {"Savings By Day", Variable}},
        receive
                {WorkerPid, Result} -> Result
        after 2000->
                timeout
        end;
accountant(WorkerPid, Variable) ->
        WorkerPid  !{self(), {Variable}},
        receive
                {WorkerPid, Result} -> Result
        after 2000->
                timeout
        end.


accountant(WorkerPid, Day, Currency) ->
        WorkerPid  !{self(), {"Daily Ballance", Day, Currency}},
        receive
                {WorkerPid, Result} -> Result
        after 2000->
                timeout
        end.


start_worker() ->
        spawn(?MODULE, worker, []).

worker() ->
        receive
                {From, {"Check Price", PetName}} ->
                        Result = check_price_of_pet(PetName),
                        From ! {self(), Result},
                        worker();
                
                {From, {"Buy Pet", PetName, SellingPrice, BuyoutPrice, Currency, Day}} ->
                        Result = buy_pet(PetName, SellingPrice, BuyoutPrice, Currency, Day),
                        From ! {self(), Result},
                        worker();
               
                {From, {"Sell Pet", PetName, Currency, Day}} ->
                        Result = sell_pet(PetName, Currency, Day),
                        From ! {self(), Result},
                        worker();
                
                {From, {"Money Income"}} ->
                        Result = get_money_in(),
                        From ! {self(), Result},
                        worker();
               
                {From, {"Money Spent"}} ->
                        Result = get_money_out(),
                        From ! {self(), Result},
                        worker();
               
                {From, {"Daily Ballance", Day, Currency}} ->
                        Result = get_money_by_day_and_currency(Day, Currency),
                        From ! {self(), Result},
                        worker();
                
                {From, {"Savings Ballance"}} ->
                        Result = get_safe_box_money(),
                        From ! {self(), Result},
                        worker();
               
                {From, {"Savings By Day", Day}} ->
                        Result = get_safe_box_money_by_day(Day),
                        From ! {self(), Result},
                        worker();
                
                {From, {"Close Shop"}} ->
                        From ! {self(), "Shop Closed"}

        end.


