type

    UnImplementedError = class(Exception);
    NotSupportedError = class(Exception);

    {
      Generic and abstract class that holds a Pascal type+value.
      Derived from TObject for extra functions.
      
      This class is meant to be derived, and since I do not going to add
      OOP into FPSH, this class is only deriveable in FPSH source code.
      
      Remember this is a generic class, so to derive, create a class with
      specialize keyword, then derive it. I may use variant type to remove
      that (also the class will no longer a generic class).
    }
    generic TFPSHObject<T> = class(TObject)
	protected
        { Converts value to string. }
        function valueToString: string;

    public
        { The value that this class holds. }
        value: T;

        { The name of the instance/variable. }
        name: string;

        { Constructor. Simple enough.
          From FreePascal documentation:
          
          "Note that allocating the memory for the object instance happens in hidden code generated as part of every constructor, not only in TObject.Create.
          The hidden code calls NewInstance."
          
          OK. }
        constructor Create(obj: T);

        { String presentation of the class.
          It should be <class name> object at <address>, holding <value>.
          Unimplemented. }
        function ToString(): string; override;

        function copy(): TFPSHObject; overload;

        procedure copy(destination: TFPSHObject); overload;
    end;

	{ Arrays. }
	generic TFPArray<T> = class(TObject)
	private
		type TArr = array of T;

	protected
		function valueToString: string;
	
	public
		value: TArr;

		constructor Create(arr: TArr);
		function Count: Integer;
	end;

	{ Forward declarations }
	TInteger = class;
	TString = class;

	_TIntArray = specialize TFPArray<TInteger>;
	TIntArray = class(_TIntArray);

	_TStringArray = specialize TFPArray<TString>;
	TStringArray = class(_TStringArray);

    { Integer }
    _TInteger = specialize TFPSHObject<integer>;
    TInteger = class(_TInteger)
	protected
        function valueToString: string;

    public
        constructor Create(str: string); overload;
    end;

    { Character. }
    _TCharacter = specialize TFPSHObject<char>;

    TCharacter = class(_TCharacter)
	protected
		function valueToString: string;

    public
		function code: TInteger;

		constructor Create(charCode: TInteger); overload;

		function upper: TCharacter;
		function lower: TCharacter;
    end;

    { String. An array of characters. }
    _TString = specialize TFPSHObject<string>;

    TString = class(_TString)
	protected
		function valueToString: string;

    public

		function plus(right: TCharacter): TString;
        function minus(right: TString): TString; overload;
		function minus(right: array of integer): TString; overload;
        function multiply(right: TInteger): TString;
        function divide(right: TString): TString;

        function quoted: TString;
        function upper: TString;
        function lower: TString;

        function indexOf(what: TString): TInteger;
        function fromIndex(what: TInteger): TCharacter;
		function count: TInteger;

		// operator[] as well.
		function contains(
			what: TString; caseSensitive: boolean = false;
			returnIndexes: boolean = false
		): TIntArray;

		function startsWith(what: TString; caseSensitive: boolean = false): boolean;
    end;
