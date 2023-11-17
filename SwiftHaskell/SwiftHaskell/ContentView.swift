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
            
            Text("Factorial of each: \( factorial_each(lista: [1,2,3,4,5,6,7,8,9,10]).joined(separator: ", ")) ")
            
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

@ForeignImportHaskell
func factorial_each(cconv: HsCallJSON, lista: [Int]) -> [String] { fatalError() }

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

struct User2 : Codable {
    let birthYear: Int,
        age: Int
}

@ForeignImportHaskell
func birthday(cconv: HsCallJSON, _ u : User) -> User { fatalError("impossible") }

func birthday (_ u : User2) -> User2 {
    let enc = JSONEncoder()
    let dec = JSONDecoder()
    do {
        var data : Data = try enc.encode(u)
        let data_len = Int64(data.count)
        return try data.withUnsafeMutableBytes { (rawPtr:UnsafeMutableRawBufferPointer) in
            // Won't change the data...
            // Can I somehow get pointer to const in the header?
            
            // Allocate buffer for result
            let buf_size = 1024000
            return try withUnsafeTemporaryAllocation(of: Int.self, capacity: 1) { size_ptr in
                size_ptr.baseAddress?.pointee = buf_size
                print("Set buf_size in address")
                
                do {
                    return try withUnsafeTemporaryAllocation(byteCount: buf_size, alignment: 1) { res_ptr in
                        
                        hbirthday(rawPtr.baseAddress, data_len, res_ptr.baseAddress, size_ptr.baseAddress)
                        
                        if let required_size = size_ptr.baseAddress?.pointee {
                            if required_size > buf_size {
                                throw HsFFIError.requiredSizeIs(required_size)
                            }
                        }
                        let new_data = Data(bytesNoCopy: res_ptr.baseAddress!, count: size_ptr.baseAddress?.pointee ?? 0, deallocator: .none)
                        print("Got JSON from call: \(String(bytes: new_data, encoding: .ascii) ?? "???")")
                        return try dec.decode(User2.self, from: new_data)
                    }
                } catch HsFFIError.requiredSizeIs(let required_size) {
                    print("Retrying with required size: \(required_size)")
                    return try withUnsafeTemporaryAllocation(byteCount: required_size, alignment: 1) { res_ptr in
                        size_ptr.baseAddress?.pointee = required_size
                        
                        hbirthday(rawPtr.baseAddress, data_len, res_ptr.baseAddress, size_ptr.baseAddress)
                        
                        return try dec.decode(User2.self, from: Data(bytesNoCopy: res_ptr.baseAddress!, count: size_ptr.baseAddress?.pointee ?? 0, deallocator: .none))
                    }
                }
            }
        }
    } catch {
        print("Error decoding JSON probably: \(error)")
        return User2(birthYear: 0, age: 0)
    }
}

enum HsFFIError: Error {
    case requiredSizeIs(Int)
}

#Preview {
    ContentView()
}
