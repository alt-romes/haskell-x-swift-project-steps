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
            Text("Hello, Haskell: \(hs_double(9))!")
        }
        .padding()
    }
}

#Preview {
    ContentView()
}
