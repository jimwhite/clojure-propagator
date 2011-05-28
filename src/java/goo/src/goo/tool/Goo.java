package goo.tool;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import com.sun.org.apache.bcel.internal.Repository;
import com.sun.org.apache.bcel.internal.classfile.*;
import com.sun.org.apache.bcel.internal.generic.*;
import com.sun.org.apache.bcel.internal.generic.FieldOrMethod;

public class Goo {
	private JavaClass clazz;
	private List<Edge> nodeEdges = new ArrayList<Edge>();
	private ConstantPoolGen constantPool;

	public Goo(String className)  {
        try {
            clazz = Repository.lookupClass(Class.forName(className));
            if (clazz == null)
                throw new InvalidClassName();
            constantPool = new ConstantPoolGen(clazz.getConstantPool());
            build();
        } catch (ClassNotFoundException e) {
            throw new InvalidClassName();
        }
	}

	void build() {
		Method [] methods = clazz.getMethods();
		if (methods == null)
			return;
		for(Method method : methods) {
			if(!(method.isAbstract() || method.isNative()))
					buildMethodEdges(method);
		}
		strip("<init>");
		strip("$");
	}

    void strip(String filterText) {
		List<Edge> newEdgeList = new ArrayList<Edge>();
		for(Edge edge : nodeEdges) {
			if (edge.containsSubstring(filterText))
				continue;
			newEdgeList.add(edge);
		}
		nodeEdges = newEdgeList;
	}


    InstructionList getInstructionListForMethod(Method method) {
		MethodGen methodGen = new MethodGen(method, clazz.getClassName(), constantPool);
		return methodGen.getInstructionList();
	}

	Set<String> getUsesForMethod(Method method) {
		HashSet<String> uses = new HashSet<String>();
		InstructionList instructions = getInstructionListForMethod(method);
		for(Iterator it = instructions.iterator(); it.hasNext(); ) {
			InstructionHandle handle = (InstructionHandle)it.next();
			Instruction instruction = handle.getInstruction();
			if (!isIncludedAccessInstruction(instruction))
				continue;
			FieldOrMethod accessInstruction = (FieldOrMethod)instruction;
			if (!isInstructionForThisClass(accessInstruction))
				continue;
			uses.add(accessInstruction.getName(constantPool));
		}
		return uses;
	}

	boolean isInstructionForThisClass(FieldOrMethod accessInstruction) {
		return clazz.getClassName().equals(accessInstruction.getClassName(constantPool));
	}

	boolean isIncludedAccessInstruction(Instruction instruction) {
		return instruction instanceof GETFIELD
			|| instruction instanceof PUTFIELD
			|| instruction instanceof INVOKESPECIAL
			|| instruction instanceof INVOKEVIRTUAL;
	}

	void buildMethodEdges(Method method) {
		Set<String> uses = getUsesForMethod(method);
		for(String use : uses) {
			nodeEdges.add(new Edge(method.getName(), use));
		}
	}

        public List<Edge> getEdges() {
		return nodeEdges;
	}

	public List<String> getFieldNodes () {
		List<String> result = new ArrayList<String>();
		Field [] fields = clazz.getFields();
		for(Field field : fields) {
			if (!field.isStatic())
				result.add(field.getName());
		}
		return result;
	}

	public List<String> getNonVisibleMethodNodes() {
		List<String> result = new ArrayList<String>();
		Method [] methods = clazz.getMethods();
		for(Method method : methods) {
			if ((method.isProtected() || method.isPrivate())
					&& !method.isStatic()
					&& !method.getName().equals("<init>"))
				result.add(method.getName());
		}
		return result;
	}

	public List<String> getPublicMethodNodes() {
		List<String> result = new ArrayList<String>();
		Method [] methods = clazz.getMethods();
		for(Method method : methods) {
			if (method.isPublic()
					&& !method.isStatic()
					&& !method.getName().equals("<init>"))
				result.add(method.getName());
		}
		return result;
	}
}
