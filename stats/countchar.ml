let main =
    let lineCounter = ref 0 in
    let charCounter = ref 0 in
    for i = 2 to 6 do
        let in_channel = open_in ((string_of_int i) ^ ".csv") in
        print_endline (input_line in_channel);
        try
        while true do
            let currLine = input_line in_channel in
            lineCounter := !lineCounter + 1;
            charCounter := !charCounter + (String.length currLine);
        done;
        with End_of_file -> () 
    done;
    print_endline (string_of_int !charCounter);
    print_endline (string_of_int !lineCounter);