// Copyright 2018 Vincenzo Ciancia.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
//
// A copy of the license is available in the file "Apache_License.txt".
// You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#nowarn "9"
namespace VoxLogicA

open Microsoft.FSharp.NativeInterop
open System.Runtime.InteropServices


type NativeArray<'T when 'T : unmanaged> = // TODO: split this in readonly and readwrite, make readwrite access possible only in certain conditions (and make sure that the handled object is owned by the current thread)
    val ptr : nativeptr<'T>
    val handle : GCHandle
    val length : int
    val mutable disposed : bool

    member inline this.Length = this.length

    member inline this.InBounds n =
        n >= 0 && n < this.Length
    
    new (p : nativeptr<'T>, l: int, o: obj) = 
        {   ptr = p
            length = l
            disposed = false
            handle = GCHandle.Alloc(o) }

    interface System.IDisposable with
        member this.Dispose() = 
            lock this (fun () -> if not this.disposed then this.disposed <- true; this.handle.Free ())

    override this.Finalize() =
        (this :> System.IDisposable).Dispose()

    member inline this.UGet n =  
        assert (this.InBounds n)              
        NativePtr.get this.ptr n
    
    member inline this.USet n v =
        assert (this.InBounds n)
        NativePtr.set this.ptr n v
    
    member inline this.Get n =
        if this.InBounds n 
        then this.UGet n 
        else raise (System.IndexOutOfRangeException())

    member inline this.Set n v =
        if this.InBounds n 
        then this.USet n v 
        else raise (System.IndexOutOfRangeException())
    
    member inline this.Iter f =
        for i = 0 to this.length - 1 do
            f (this.UGet i)

    member inline this.Iteri f =
        for i = 0 to this.length - 1 do
            f i (this.UGet i)
    
    member inline this.Apply f =
        for i = 0 to this.length - 1 do
            this.USet i (f (this.UGet i))

    member inline this.CopyFrom (other : NativeArray<'T>) =
        assert (other.Length = this.Length)
        for i = 0 to this.length - 1 do
            this.USet i (other.UGet i)    

    member inline this.Applyi f =
        for i = 0 to this.length - 1 do
            this.USet i (f i (this.UGet i))

    member inline this.Replace f =
        for i = 0 to this.length - 1 do
            this.USet i (f i)
    
    member inline this.Fill v =
        for i = 0 to this.length - 1 do
            this.USet i v
