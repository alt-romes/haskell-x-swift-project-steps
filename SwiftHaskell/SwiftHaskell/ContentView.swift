//
//  ContentView.swift
//  SwiftHaskell
//
//  Created by Rodrigo Mesquita on 09/11/2023.
//

import SwiftUI
import HaskellFramework.MyForeignLib_stub

struct User: Codable {
    let name: String
    let age: Int
}

struct Rect {
    let width: Int
    let height: Int
}

func wrap_give_rect() -> Rect {
    let y = give_rect()
    return y!.load(as: Rect.self)
}

// birthday(user: User(name: "Anton", age: 33)) = User(name: "Anton", age: 34)
func birthday (user : User) -> User {
    let enc = JSONEncoder()
    let dec = JSONDecoder()
    do {
        var data : Data = try enc.encode(user)
        let data_len = Int64(data.count)
        return try data.withUnsafeMutableBytes { (rawPtr:UnsafeMutableRawBufferPointer) in

            // Allocate buffer for result
            let buf_size = 1024000

            return try withUnsafeTemporaryAllocation(of: Int.self, capacity: 1) { size_ptr in
                size_ptr.baseAddress?.pointee = buf_size

                do {
                    return try withUnsafeTemporaryAllocation(byteCount: buf_size, alignment: 1) {
 res_ptr in

                        c_birthday(rawPtr.baseAddress, data_len, res_ptr.baseAddress, size_ptr.baseAddress)

                        if let required_size = size_ptr.baseAddress?.pointee {
                            if required_size > buf_size {
                                throw HsFFIError.requiredSizeIs(required_size)
                            }
                        }
                        return try dec.decode(User.self, from: Data(bytesNoCopy: res_ptr.baseAddress!, count: size_ptr.baseAddress?.pointee ?? 0, deallocator: .none))
                    }
                } catch HsFFIError.requiredSizeIs(let required_size) {
                    print("Retrying with required size: \(required_size)")
                    return try withUnsafeTemporaryAllocation(byteCount: required_size, alignment:
 1) { res_ptr in
                        size_ptr.baseAddress?.pointee = required_size

                        c_birthday(rawPtr.baseAddress, data_len, res_ptr.baseAddress, size_ptr.baseAddress)

                        return try dec.decode(User.self, from: Data(bytesNoCopy: res_ptr.baseAddress!, count: size_ptr.baseAddress?.pointee ?? 0, deallocator: .none))
                    }
                }
            }
        }
    } catch {
        print("Error decoding JSON probably: \(error)")
        return User(name: "", age: 0)
    }
}

enum HsFFIError: Error {
    case requiredSizeIs(Int)
}

struct ContentView: View {
    var body: some View {
        VStack {
            let user = birthday(user: User(name: "Ellie", age: 24))
            let rect = wrap_give_rect()
            Text("Post-birthday, \(user.name) is: \(user.age)!")
            Text("myrect: width is \(rect.width) and height is \(rect.height)!")
        }
        .padding()
    }
}

#Preview {
    ContentView()
}
