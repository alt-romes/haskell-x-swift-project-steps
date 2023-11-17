//
//  ContentView.swift
//  SwiftHaskell
//
//  Created by Rodrigo Mesquita on 09/11/2023.
//

import SwiftUI
import HaskellFramework.MyForeignLib_stub
import HsFFIMacro

struct ContentView: View {
    @State private var u:User = User(birthYear: 1999, age: 24)
    
    var body: some View {
        VStack {
            Text("Hello, Haskell: \(hs_factorial(5))!")
            Text("Hello, User: \(u.age)!")
            Button {
                u = birthday(u)
            } label: {
                Label("It's my birthday!", systemImage: "birthday.cake")
            }
        }
        .padding()
    }
}

struct User: Codable {
    let birthYear: Int64,
        age: Int64
}

func mkSimpleUser (age: Int64) -> User {
    let x:UnsafeMutableRawPointer = mk_simple_user(age)!
    return (x).load(fromByteOffset: 8, as: User.self)
}

func mkSimpleUserMarshal (age: Int64) -> User {
    withUnsafeTemporaryAllocation(of: User.self, capacity: 1) { ptr in
        marshalUser(ptr.baseAddress)
        
        return ptr.first!
    }
}

@ForeignImportHaskell
func birthday(cconv: HsCallJSON, _ u : User) -> User { fatalError("impossible") }

#Preview {
    ContentView()
}
