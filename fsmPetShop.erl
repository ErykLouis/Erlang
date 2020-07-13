-module(fsmPetShop).
-define(ERROR_PET, "Sorry, no pet with this name!").
-behaviour(gen_fsm).

-record(store, {item, 
                price, 
                amount}).
-record(funds, {  
                  money_in, 
                  day, 
                  currency,
                  money_out}).
-record(state, {name,
                salary,
                pets_sold,
                money_left_to_pay,
                selling_pet,
                money}).

-export([
        insert_record_in_table/1, 
        check_price_of_pet/1, 
        sell_pet/3
        ]).
-export([start_link/1,
        init/1,
        check_pet/1,
        available/2,
        at_home/2,
        informing_customer/2,
        make_sell/2,
        give_receipt/2,
        handle_event/3,
        handle_sync_event/4
        ]).


insert_record_in_table(Record) ->
    Fun = fun() -> mnesia:write(Record)
          end,
    mnesia:transaction(Fun).

%%FSM PART
start_link(Name) ->
gen_fsm:start_link(?MODULE,[Name],[]).

init(Name) ->
io:format("Time to work!~n"),
{ok, at_home, #state{name = Name,
                       salary = 1000,
                       pets_sold = 0,
                       money_left_to_pay =0,
                       selling_pet = none,
                       money =0}}.


%inital state of the emp, at home
at_home({open_shop}, #state{} = S) ->
    io:format("Store is now open!"),
    {next_state, available, S};
at_home(Event, #state{} = S) ->
io:format("Unkown Command ~p ~n", [Event]),
{next_state, at_home, S}.


%%functions that help the available state
check_price_of_pet(PetName) ->
        Fun = fun() ->
                case mnesia:read({table_store, PetName}) of
                    [#store{price = P}] ->
                        {PetName, P};
                    [] -> 
                        undefined
                end
        end,
        mnesia:transaction(Fun).

check_pet(PetName) ->
    Fun = fun() ->
                case mnesia:read({table_store, PetName}) of
                    [#store{amount = Amount}] ->
                        case Amount of 
                            0 -> 
                                undefined;
                            _ ->
                                #store{}
                        end;
                    [] -> 
                        undefined
                end
        end,
        mnesia:transaction(Fun).

%% Available state
available({inform, PetName}, #state{} = S) ->
    Result = check_price_of_pet(PetName),
    case Result of 
        undefined -> 
            io:format(?ERROR_PET);
        _->
            io:format("The price of ~p is ~p UM ~n",[PetName], [Result]),
            {next_state, informing_customer, S#state{selling_pet = PetName}}
    end;
available({request, PetName}, #state{} = S) ->
    Result = check_pet(PetName),
    case Result of
        undefined -> 
            io:format(?ERROR_PET);
        _->
            io:format("Let me prepare ~p~n",[PetName]),
            {next_state, make_sell, S#state{selling_pet = PetName, money_left_to_pay = Result#store.price}}
    end;
available({close_shop}, #state{} = S) ->
    io:format("Shop closed!"),
    {next_state, at_home, S}.


%%informing customer state
informing_customer({request, PetName}, #state{} = S) ->
      Result = check_pet(PetName),
      case Result of 
          undefined ->
              io:format("The pet ~p is out of stock ~n",[PetName]);
          _ ->
            io:format("Let me prepare ~p~n",[PetName]),
            {next_state, make_sell, S#state{money_left_to_pay = Result#store.price }}
        end;
informing_customer({bye}, #state{} = S) ->
    io:format("Bye!"),
    {next_state, make_sell, S#state{selling_pet = none}}.



%%making the sale state (we give change)
make_sell({make_pay, CurrentPay}, #state{money_left_to_pay = Needed,  money = Money} = S) ->
    NewLeftToPay = Needed - CurrentPay,
    case NewLeftToPay > 0 of
        true ->
            io:format("money_left_to_pay ~p~n", [Needed - NewLeftToPay]),
            {next_state, give_receipt, S#state{money_left_to_pay = 0, money = Money + CurrentPay + NewLeftToPay}}; %%if NewLeftToPay < 0 than we give the change
        false ->
            io:format("money_left_to_pay ~p~n", [Needed - NewLeftToPay]),
            {next_state, make_sell, S#state{money_left_to_pay = NewLeftToPay}}
    end.

%%selling function
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


%%giving the receipt state
give_receipt({bye, Currency, Day}, #state{ selling_pet = PetName, pets_sold = PetsSold} = S) ->
    Result = sell_pet(PetName, Currency, Day),
    case Result of
        undefined -> 
            io:format("Never gonna get here!");
        _ ->
            {next_state, available, S#state{selling_pet = none, pets_sold = PetsSold + 1 }}
    end.
    

handle_event(needToParty, _StateName, #state{} = S) ->
{next_state, at_home, S}.

% handle_sync_event(Event, From, StateName, StateData)
handle_sync_event(needToParty, _From, _StateName, #state{} = S) ->
{next_state, at_home, S}.
