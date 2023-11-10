//
//  ContentView.swift
//  SwiftHaskell
//
//  Created by Rodrigo Mesquita on 09/11/2023.
//

import SwiftUI

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
