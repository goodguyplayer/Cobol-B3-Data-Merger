# Cobol-B3-Data-Merger
A tool that takes a historical quote data from B3 [(Link here)](https://www.b3.com.br/en_us/market-data-and-indices/data-services/market-data/historical-data/equities/historical-quote-data/) and outputs it into a singular csv file. Mostly a PoC

## Usage
The tool was developed using OpenCobolIDE.
- Copybooks set to CPY folder
- Data files stored in DATA folder (git ignored)
- Configure a list of files you wish to load in MAINCODE.cbl, line 17
- Configure the output file on line 49

## Logic
- MAINCODE.cbl loads the file list, where every file to load is defined in every line
- for every line, call MODLRDWR.cbl
- In MODLRDWR.cbl, for the given input file, read every line
- If line starts with 01, proceed to read with copybook
- Alter the data with copybook and outputs on output file

## Contributing

Pull requests are welcome. For major changes, please open an issue first
to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License

[MIT](https://choosealicense.com/licenses/mit/)
