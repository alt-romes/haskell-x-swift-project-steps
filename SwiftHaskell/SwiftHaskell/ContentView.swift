//
//  ContentView.swift
//  SwiftHaskell
//
//  Created by Rodrigo Mesquita on 09/11/2023.
//

import SwiftUI
import HaskellFramework.MyForeignLib_stub

struct ContentView: View {
    var body: some View {
        VStack {
            Text("Hello, Haskell: \(hs_factorial(5))!")
            Text("Hello, User: \(mkSimpleUser(age: 25).birthYear)!")
        }
        .padding()
    }
}

struct User {
    let birthYear: Int64,
        age: Int64
}

func mkSimpleUser (age: Int64) -> User {
    let x:UnsafeMutableRawPointer = mk_simple_user(age)!
    let y:UnsafeMutableRawPointer = derefsp(x)!
    return (y).load(fromByteOffset: 8, as: User.self)
}

#Preview {
    ContentView()
}
