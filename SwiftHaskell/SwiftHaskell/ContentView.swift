//
//  ContentView.swift
//  SwiftHaskell
//
//  Created by Rodrigo Mesquita on 09/11/2023.
//

import SwiftUI
import HaskellFramework.MyForeignLib_stub

struct User {
    let name: String
    let age: Int
}

// birthday(user: User(name: "Anton", age: 33)) = User(name: "Anton", age: 34)
func birthday(user: User) -> User {
    // Calls Haskell function...
}

struct ContentView: View {
    var body: some View {
        VStack {
            Text("Hello, Haskell: \(hs_factorial(5))!")
        }
        .padding()
    }
}

#Preview {
    ContentView()
}
