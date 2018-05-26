export class A {
  private tag: "A";
  jsonpath: string;
  constructor(jsonpath: string) {
    this.jsonpath = jsonpath;
  }
  get at () {
    return {
      an_integer: new TsInt(`${this.jsonpath}.an_integer`),
      test_enum: new Asdf(`${this.jsonpath}.test_enum`),
      an_array_of_integers: new Array_TsInt(`${this.jsonpath}.an_array_of_integers`),
      an_array_of_bs: new Array_B(`${this.jsonpath}.an_array_of_bs`),
      an_array_of_cs: new Array_C(`${this.jsonpath}.an_array_of_cs`)
    }
  }
}
export class B {
  private tag: "B";
  jsonpath: string;
  constructor(jsonpath: string) {
    this.jsonpath = jsonpath;
  }
  get at () {
    return {
      b_field: new Array_TsInt(`${this.jsonpath}.b_field`)
    }
  }
}
export class B__map__ {
  private tag: "B__map__";
  jsonpath: string;
  constructor(jsonpath: string) {
    this.jsonpath = jsonpath;
  }
  get at () {
    return {
      b_field: new Array_Array_TsInt(`${this.jsonpath}.b_field`)
    }
  }
}
export class C {
  private tag: "C";
  jsonpath: string;
  constructor(jsonpath: string) {
    this.jsonpath = jsonpath;
  }
  get at () {
    return {
      an_array_of_es: new Array_Array_TsInt(`${this.jsonpath}.an_array_of_es`),
      an_array_of_bs: new Array_B(`${this.jsonpath}.an_array_of_bs`)
    }
  }
}
export class C__map__ {
  private tag: "C__map__";
  jsonpath: string;
  constructor(jsonpath: string) {
    this.jsonpath = jsonpath;
  }
  get at () {
    return {
      an_array_of_es: new Array_Array_Array_TsInt(`${this.jsonpath}.an_array_of_es`),
      an_array_of_bs: new Array_Array_B(`${this.jsonpath}.an_array_of_bs`)
    }
  }
}
export class Array_Array_Array_TsInt {
  private tag: "Array_Array_Array_TsInt";
  jsonpath: string;
  constructor(jsonpath: string) {
    this.jsonpath = jsonpath;
  }
  index(i: number) {
    return new Array_Array_TsInt(`${this.jsonpath}[${i}]`);
  }
  slice(start: number, end: number) {
    return new Array_Array_Array_TsInt(`${this.jsonpath}[${start}:${end}]`);
  }
  
}
export class Array_Array_B {
  private tag: "Array_Array_B";
  jsonpath: string;
  constructor(jsonpath: string) {
    this.jsonpath = jsonpath;
  }
  index(i: number) {
    return new Array_B(`${this.jsonpath}[${i}]`);
  }
  slice(start: number, end: number) {
    return new Array_Array_B(`${this.jsonpath}[${start}:${end}]`);
  }
  
}
export class Array_Array_TsInt {
  private tag: "Array_Array_TsInt";
  jsonpath: string;
  constructor(jsonpath: string) {
    this.jsonpath = jsonpath;
  }
  index(i: number) {
    return new Array_TsInt(`${this.jsonpath}[${i}]`);
  }
  slice(start: number, end: number) {
    return new Array_Array_TsInt(`${this.jsonpath}[${start}:${end}]`);
  }
  
}
export class Array_B {
  private tag: "Array_B";
  jsonpath: string;
  constructor(jsonpath: string) {
    this.jsonpath = jsonpath;
  }
  index(i: number) {
    return new B(`${this.jsonpath}[${i}]`);
  }
  slice(start: number, end: number) {
    return new Array_B(`${this.jsonpath}[${start}:${end}]`);
  }
  get map() {
    return {
      b_field: new B__map__(`${this.jsonpath}[*]`).at.b_field
    };
  }
}

export class Array_C {
  private tag: "Array_C";
  jsonpath: string;
  constructor(jsonpath: string) {
    this.jsonpath = jsonpath;
  }
  index(i: number) {
    return new C(`${this.jsonpath}[${i}]`);
  }
  slice(start: number, end: number) {
    return new Array_C(`${this.jsonpath}[${start}:${end}]`);
  }
  get map() {
    return {
      an_array_of_es: new C__map__(`${this.jsonpath}[*]`).at.an_array_of_es,
      an_array_of_bs: new C__map__(`${this.jsonpath}[*]`).at.an_array_of_bs
    };
  }
}
export class Array_TsInt {
  private tag: "Array_TsInt";
  jsonpath: string;
  constructor(jsonpath: string) {
    this.jsonpath = jsonpath;
  }
  index(i: number) {
    return new TsInt(`${this.jsonpath}[${i}]`);
  }
  slice(start: number, end: number) {
    return new Array_TsInt(`${this.jsonpath}[${start}:${end}]`);
  }
  
}
export class TsInt {
  private tag: "TsInt";
  jsonpath: string;
  constructor(jsonpath: string) {
    this.jsonpath = jsonpath;
  }
  
}
export class Asdf {
  private tag: "Asdf";
  jsonpath: string;
  constructor(jsonpath: string) {
    this.jsonpath = jsonpath;
  }
  
}